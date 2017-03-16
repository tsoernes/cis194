{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Fibbonacci () where

-- Ex 1
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Ex 2
--
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

fibs2' ::[Integer]
fibs2' = 0 : 1 : next fibs2'
  where
    next (n1 : r@(n2:_)) = (n1+n2) : next r

-- Ex 3
data Stream a = Stream  a (Stream a)

instance Show a => Show (Stream a) where
  show a = show $ take 5 $ streamToList a

streamToList :: Stream a -> [a]
streamToList (Stream e r) = e : streamToList r

-- Ex 4
--
streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream e r) = Stream (f e) (streamMap f r)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream r (streamFromSeed f r)
  where
    r = f a

-- Ex 5
--
nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

ruler :: Stream Integer
ruler = interRep 0
  where
    interRep :: Integer -> Stream Integer
    interRep n = interleaveStreams (streamRepeat n) (interRep (n+1))


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream e1 r1) b = Stream e1 $ interleaveStreams b r1

-- Ex 6

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Stream n (streamRepeat 0)
  negate = streamMap (*(-1))
  (+) (Stream e1 r1) (Stream e2 r2) = Stream (e1+e2) (r1 + r2)
  (*) (Stream e1 r1) b@(Stream e2 r2) = Stream (e1*e2) (streamMap (*e1) r2 + r1*b)
  abs = undefined
  signum = undefined

fibs3 :: Stream Integer
fibs3 = undefined

-- Ex 7

fib4 :: Integer -> Integer
fib4 = undefined
