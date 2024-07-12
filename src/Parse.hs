-- | Parsec like commbinators for a LISP reader.
--
-- References:
--   https://stackoverflow.com/questions/20660782/writing-a-parser-from-scratch-in-haskell
--   https://hackage.haskell.org/package/parsec
--   https://en.wikipedia.org/wiki/Lisp_reader
module Parse where

import Data.String (IsString (fromString))
import MonadT
  ( ErrorT (runErrorT),
    Identity (runIdentity),
    MonadError (..),
    MonadState (..),
    StateT (runStateT),
  )
import Text.Printf (printf)

type Error = String

-- | The State of the parser
-- `stream` is what's left of the input
-- `row` and `col` tell you where the cursor is
-- (i.e. the next char to be consumed)
data State = State
  { stream :: String,
    row :: Integer,
    col :: Integer
  }

instance IsString State where
  fromString st = State {stream = st, row = 0, col = 0}

-- | Parser type
type Parser = ErrorT Error (StateT State Identity)

runParser :: Parser a -> State -> Either Error a
runParser p st =
  if not . null $ stream st'
    then Left (printf "unexpected symbols at row %d, col %d" (row st') (col st'))
    else a
  where
    (a, st') = runIdentity (runStateT (runErrorT p) st)

-- | Alternative
-- Since we are doing things from scratch...
class (Applicative f) => Alternative f where
  -- | The identity of `<|>`
  -- Huh? Elaborating:
  --   `empty <|> p` and `p` are equivalent
  empty :: f a

  -- | If the first one fails, try the second one
  (<|>) :: f a -> f a -> f a

  some :: f a -> f [a]
  some v = (:) <$> v <*> many v

  many :: f a -> f [a]
  many v = some v <|> pure []

instance Alternative Parser where
  empty = throwError "something went horribly wrong (:"
  p <|> g = p `catchError` \(_ :: Error) -> g

-- | Peek at the next character and return successfully
-- (consuming the character) if it satisfies a predicate
satisfy :: (Char -> Bool) -> String -> Parser Char
satisfy check errorMsg = do
  st <- get
  case stream st of
    [] -> throwError (printf "%s, but stream ended prematurely" errorMsg :: Error)
    c : cs
      | check c -> do
          put $ update st c cs
          return c
      | otherwise ->
          throwError
            ( printf
                "%s, but instead got '%c' at row %d, column %d"
                errorMsg
                c
                (row st)
                (col st) ::
                Error
            )
  where
    update :: State -> Char -> [Char] -> State
    update st c cs =
      let withRest = st {stream = cs}
       in if c == '\n'
            then withRest {col = 0, row = row st + 1}
            else withRest {col = col st + 1}

-- | Run a parser but if it fails revert the state to it's original
try :: Parser a -> Parser a
try p = do
  original :: State <- get
  p `catchError` \(_ :: Error) -> do
    put original
    empty

zeroOrOne :: (Monoid a) => Parser a -> Parser a
zeroOrOne p = try p <|> pure mempty

char :: Char -> Parser Char
char c = satisfy (== c) (printf "expected '%c'" c)

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs) (printf "expected one of \"%s\"" cs)

string :: String -> Parser String
string (c : cs) = (:) <$> char c <*> string cs
string [] = pure []

-- | Wishful thinking: `SExp` is the output of a lisp reader :)
data SExp
  = Atom AKind
  | Pair {car :: SExp, cdr :: SExp}
  | Nil
  deriving (Eq)

instance Show SExp where
  show e = case listify e of
    Right (x : xs) -> "(" ++ foldl (\b a -> b ++ " " ++ show a) (show x) xs ++ ")"
    Left _ -> case e of
      Atom a -> show a
      Pair car cdr -> "(" ++ show car ++ " . " ++ show cdr ++ ")"
      Nil -> ""
    Right [] -> "()"

cddr :: SExp -> SExp
cddr = cdr . cdr

cadr :: SExp -> SExp
cadr = car . cdr

caddr :: SExp -> SExp
caddr = car . cdr . cdr

cadddr :: SExp -> SExp
cadddr = car . cdr . cdr . cdr

listify :: SExp -> Either Error [SExp]
listify (Pair car cdr) = (:) <$> Right car <*> listify cdr
listify Nil = Right []
listify e = Left $ "S-expression is not a valid list: " ++ show e

-- | Atomic kinds
data AKind
  = Symbol String
  | IntLiteral Integer
  | BoolLiteral Bool
  deriving (Eq)

instance Show AKind where
  show (Symbol n) = n
  show (IntLiteral i) = show i
  show (BoolLiteral b) = show b

-- | Whitespaces
ws :: Parser String
ws = many $ oneOf " \n"

digits :: String
digits = "0123456789"

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

misc :: String
misc = "-!?=*+"

parens :: Parser a -> Parser a
parens parseA = ws *> char '(' *> ws *> parseA <* ws <* char ')' <* ws

integer :: Parser AKind
integer = toInt <$> (ws *> try negative <|> try positive <* ws)
  where
    negative = (:) <$> char '-' <*> positive
    positive = some (oneOf digits)
    toInt = IntLiteral . read

boolean :: Parser AKind
boolean = toBool <$> (ws *> try (string "#t") <|> try (string "#f") <* ws)
  where
    toBool = BoolLiteral . (== "#t")

symbol :: Parser AKind
symbol = Symbol <$> (ws *> some (oneOf $ alphabet ++ digits ++ misc) <* ws)

-- | Atom
atom :: Parser SExp
atom = Atom <$> try integer <|> try boolean <|> try symbol

-- Pair
pair :: Parser SExp
pair = parens $ Pair <$> sexp <* ws <* char '.' <* ws <*> sexp

-- | Nil
nil :: Parser SExp
nil = Nil <$ (ws *> zeroOrOne (string "'") *> string "()" <* ws)

-- | LISP list constructor, i.e. (x ...)
-- Should satisfy: (a b c) is equivalent to (a . (b . (c . '())))
list :: Parser SExp
list = parens $ do
  elements <- some (ws *> sexp <* ws)
  return $ foldr Pair Nil elements

-- | Reader quote
quote :: Parser SExp
quote = inject <$> (ws *> char '\'' *> sexp <* ws)
  where
    inject x = Pair (Atom (Symbol "quote")) (Pair x Nil)

-- | Reader quasiquote
quasiquote :: Parser SExp
quasiquote = inject <$> (ws *> char '`' *> sexp <* ws)
  where
    inject x = Pair (Atom (Symbol "quasiquote")) (Pair x Nil)

-- | Reader unquote
unquote :: Parser SExp
unquote = inject <$> (ws *> char ',' *> sexp <* ws)
  where
    inject x = Pair (Atom (Symbol "unquote")) (Pair x Nil)

-- | All the reader macros
readerMacros :: Parser SExp
readerMacros = try quote <|> try quasiquote <|> try unquote

sexp :: Parser SExp
sexp =
  try nil
    <|> try readerMacros
    <|> try atom
    <|> try pair
    <|> list

-- Otherthings to support:
--   - [ ] Support splatting `,@`
