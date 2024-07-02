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
runParser p s = fst $ runIdentity (runStateT (runErrorT p) s)

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
  empty = throwError "empty"
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
  p `catchError` \(e :: Error) -> do
    put original
    throwError e

char :: Char -> Parser Char
char c = satisfy (== c) (printf "expected '%c'" c)

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs) (printf "expected one of \"%s\"" cs)

string :: String -> Parser String
string [] = pure []
string (c : cs) = (:) <$> char c <*> string cs

-- | Wishful thinking: `SExp` is the output of a lisp reader :)
data SExp
  = Atom String
  | Pair {car :: SExp, cdr :: SExp}
  | Nil
  deriving (Show)

whitespaces :: Parser String
whitespaces = many $ oneOf " \n"

allowedAtomChars :: String
allowedAtomChars = "abcdefghijklmnopqrstuvwxyz"

parens :: Parser a -> Parser a
parens parseA = dropFirstAndLast <$> char '(' <*> parseA <*> char ')'
  where
    dropFirstAndLast _ a _ = a

atom :: Parser SExp
atom = makeAtom <$> whitespaces <*> some (oneOf allowedAtomChars) <*> whitespaces
  where
    makeAtom _ a _ = Atom a

pair :: Parser SExp
pair = parens $ makePair <$> sexp <*> whitespaces <*> char '.' <*> whitespaces <*> sexp
  where
    makePair car _ _ _ cdr = Pair {car = car, cdr = cdr}

nil :: Parser SExp
nil = makeNil <$> string "'()"
  where
    makeNil _ = Nil

sexp :: Parser SExp
sexp = try atom <|> try nil <|> try pair
