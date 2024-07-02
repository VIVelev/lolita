-- | Parsec like commbinators for a LISP reader.
--
-- References:
--   https://stackoverflow.com/questions/20660782/writing-a-parser-from-scratch-in-haskell
--   https://hackage.haskell.org/package/parsec
--   https://en.wikipedia.org/wiki/Lisp_reader
module Parse where

import MonadT (ErrorT (runErrorT), Identity (runIdentity), MonadError (..), MonadState (..), StateT (runStateT))
import Text.Printf (printf)

type Error = String

type Stream = String

-- | Parser type
type Parser = ErrorT Error (StateT Stream Identity)

runParser :: Parser a -> Stream -> Either Error a
runParser p s = fst $ runIdentity (runStateT (runErrorT p) s)

-- | Alternative
-- Since we are doing things from scratch...
class (Applicative f) => Alternative f where
  -- | The identity of `<|>`
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

-- | Peek at the next character and return successfully if it satisfies a predicate
satisfy :: (Char -> Bool) -> String -> Parser Char
satisfy check errorMsg = do
  s <- get
  case s of
    [] -> throwError "end of stream"
    c : cs
      | check c -> do
          put cs
          return c
      | otherwise -> throwError (printf "%s, but instead got %c" errorMsg c :: String)

char :: Char -> Parser Char
char c = satisfy (== c) (printf "must equal %c" c)

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs) (printf "must be one of \"%s\"" cs)

string :: String -> Parser String
string [] = pure []
string (c : cs) = (:) <$> char c <*> string cs

parens :: Parser a -> Parser a
parens parseA = dropFirstAndLast <$> char '(' <*> parseA <*> char ')'
  where
    dropFirstAndLast _ a _ = a

-- | Wishful thinking: `SExp` is the output of a lisp reader :)
data SExp
  = Atom String
  | Pair {car :: SExp, cdr :: SExp}
  | Nil
  deriving (Show)

whitespaces :: Parser String
whitespaces = many (char ' ')

allowedAtomChars :: String
allowedAtomChars = "abcdefghijklmnopqrstuvwxyz"

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
sexp = atom <|> pair <|> nil
