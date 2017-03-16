{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Data.Maybe

import ExprT
import Parser
import qualified StackVM as SVM

main :: IO ()
main = do
  let testEval = eval testExpr1 == 20
      testEvalStr1 = evalStr testExpr2 == Just 14
      testEvalStr2 = isNothing $ evalStr testExpr3
  print testEval
  print testEvalStr1
  print testEvalStr2


eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp Lit Add Mul str

testExpr1 = Mul (Add (Lit 2) (Lit 3)) (Lit 4)
testExpr2 = "2+3*4"
testExpr3 = "2+3*"

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit i = i
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit i = i > 0
  add a b = a || b
  mul a b = a && b

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit i = Mod7 $ mod i 7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a+b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a*b) 7

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSAT     = testExp :: Maybe Mod7


instance Expr SVM.Program where
  lit i = [SVM.PushI i]
  add a b = a ++ b ++ [SVM.Add]
  mul a b = a ++ b ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul

exec :: String -> Either String SVM.StackVal
exec = SVM.stackVM . fromMaybe [] . compile
