{-# LANGUAGE LambdaCase #-}

-- Premature optimization: maybe I should only export some names?
module Parse where

import MonadT (ErrorT, Identity, MonadError (..))

type Error = String

type Stream = String

-- | Parser type
-- Think of a parser as function that takes in the stream,
-- consumes enough to produce a value of type `a` and returns
-- the rest of the stream with the production; or, it fails.
newtype Parser a = Parser {runParser :: Stream -> ErrorT Error Identity (Stream, a)}

--                                                             ^ this is a bit ugly

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (s', a) <- p s
    return (s', f a)

instance Applicative Parser where
  pure a = Parser (\s -> return (s, a))
  Parser ff <*> Parser aa = Parser $ \s -> do
    (s', f) <- ff s
    (s'', a) <- aa s'
    return (s'', f a)

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
  empty = Parser $ \_ -> throwError "empty"
  (Parser p) <|> (Parser g) = Parser $ \s ->
    p s `catchError` \(_ :: Error) -> g s

-- | Peek at the next character and return successfully if it satisfies a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy check = Parser $ \case
  [] -> throwError "end of stream"
  c : cs
    | check c -> return (cs, c)
    | otherwise -> throwError $ "did not satisfy, got " ++ show c

char :: Char -> Parser Char
char c = satisfy (== c)

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

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
