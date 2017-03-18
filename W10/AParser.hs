{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Data.Char
import Control.Monad (void)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail


char :: Char -> Parser Char
char c = satisfy (== c)


posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


instance Functor Parser where
  fmap f parser = Parser g
    where
      g inp = runParser parser inp >>= return . first f


instance Applicative Parser where
  -- | Consumes no input and always returns 'a'
  pure a = Parser f
    where
      f inp = Just (a, inp)

  -- | The first parser produces (Maybe) a function, which is applied to
  --   the result of calling the second parser on the remaining input.
  (<*>) p1 p2 = Parser f
    where
      f inp = runParser p1 inp >>= \(g, remains) -> runParser (fmap g p2) remains

-- | Apply a function to the first value in a tuple
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)


abParser :: Parser (Char, Char)
abParser = (\c1 c2 -> (c1, c2)) <$> char 'a' <*> char 'b'


abParser_ :: Parser ()
abParser_ = void abParser


intPair :: Parser [Integer]
intPair = (\i1 _ i2 -> [i1, i2]) <$> posInt <*> char ' ' <*> posInt


instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) p1 p2 = Parser f
    where
      f inp = runParser p1 inp <|> runParser p2 inp

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
