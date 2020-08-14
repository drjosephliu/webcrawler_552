module Parser where

import Prelude hiding (filter)
import Control.Applicative
import Data.Char

-- ============================================================================
-- | DATATYPES
-- ============================================================================

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
   fmap = liftA

instance Applicative Parser where
   pure x    = P (\cs -> [ (x,cs) ])
   p1 <*> p2 = P (\cs -> do (f, cs')  <- doParse p1 cs
                            (x, cs'') <- doParse p2 cs'
                            return (f x, cs''))

instance Monad Parser where
    return = pure
    p1 >>= f = P (\cs -> let [(a, s)] = doParse p1 cs
                         in doParse (f a) s)

instance Alternative Parser where
  -- the parser that always fails
  empty     = P $ const []
  -- | Combine two parsers together in parallel, but only use the
  -- first result. This means that the second parser is used only
  -- if the first parser completely fails.
  p1 <|> p2 = P $ \cs -> case doParse (p1 `choose` p2) cs of
                          []   -> []
                          x:_ -> [x]

-- ============================================================================
-- | METHODS
-- ============================================================================

-- | Calls parser
doParse :: Parser a -> String -> [(a, String)]
doParse (P p) = p

-- | Return the next character from the input
get :: Parser Char
get = P $ \cs -> case cs of
                (x:xs) -> [(x, xs)]
                []     -> []

-- | Combine two parsers together in parallel, producing all
-- possible results from either parser.
choose :: Parser a -> Parser a -> Parser a
p1 `choose` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

-- | Filter the parsing results by a predicate
filter :: (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> [ (x,s') | (x,s') <- doParse p s, f x ]

-- | Return the next character if it satisfies the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = filter p get

-- | Parses and returns the specified character
-- succeeds only if the input is exactly that character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parses and returns the specified string. Allows for upper and lowercases.
-- Succeeds only if the input is the given string
string :: String -> Parser String
string = foldr (\c p -> if isAlpha c
                        then if isLower c
                             then (:) <$> (char c <|> char (toUpper c)) <*> p 
                             else (:) <$> (char c <|> char (toLower c)) <*> p 
                        else (:) <$> char c <*> p) (pure "")


-- | Parsers for specific sorts of characters
alpha, space, digit :: Parser Char
alpha = satisfy isAlpha
space = satisfy isSpace
digit = satisfy isDigit

-- | Trims trailing whitespace for a given parser
wsP :: Parser a -> Parser a
wsP p = p <* many space

-- | Pares every character until it sees string x
lookahead :: String -> Parser Char
lookahead x = P $ \s -> if (not.null) (zip x s) && all f (zip x s)
                        then []
                        else case s of
                            [] -> []
                            (c:cs) -> [(c, cs)]
    where f = (\(x1,x2) -> x1 == x2)
          
