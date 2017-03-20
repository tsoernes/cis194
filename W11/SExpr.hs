{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser

import Data.Char
import Control.Applicative

main :: IO ()
main = do
  let str = "ABCdefGH"
      str2 = "foo33fA"
      str3 = "2bad"
      str4 = "5"
      str5 = "foo3"
      str6 = "(bar (foo) 3 5 874)"
      str7 = "(((lambda x (lambda y (plus x y))) 3) 5)"
      str8 = "( lots of ( spaces in ) this ( one ) )"
  print $ runParser (zeroOrMore (satisfy isUpper)) str
  print $ runParser (oneOrMore (satisfy isUpper)) str

  print $ runParser ident str2
  print $ runParser ident str3

  print $ runParser parseSEExpr str4
  print $ runParser parseSEExpr str5
  print $ runParser parseSEExpr str6
  print $ runParser parseSEExpr str7
  print $ runParser parseSEExpr str8

data Hole = Hole

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSEExpr :: Parser SExpr
parseSEExpr = spaces *>
              (A <$> parseAtom <|> parseComb)
              <* spaces

parseComb :: Parser SExpr
parseComb = char '(' *>  (Comb <$> oneOrMore parseSEExpr) <* char ')'

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident
