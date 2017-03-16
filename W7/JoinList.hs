{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer


main :: IO ()
main = do
  let a = scoreSizeLine "a"
      b = scoreSizeLine "b"
      c = scoreSizeLine "c"
      d = scoreSizeLine "d"
      e = scoreSizeLine "e"
      f = scoreSizeLine "f"

      g = a+++b
      h = c+++d
      i = e+++f

      j = g+++h
      jli = i+++j
      li = jlToList jli

  -- Test indexJ
  print $ map (`indexJ` jli) [0..6]
  print $ map (li !!?) [0..6]

  -- Test dropJ
  print $ map (`drop` jlToList jli) [0..6]
  print $ map (\x -> jlToList $ dropJ x jli) [0..6]

  -- Test takeJ
  print $ map (`take` jlToList jli) [0..6]
  print $ map (\x -> jlToList $ takeJ x jli) [0..6]

  -- Test scoreString
  print $ scoreLine "yay " +++ scoreLine "haskell!"

  -- Test toString
  print $ toString jli

  -- Test fromString
  -- let tString = "e\n\nf\nab q\nc\nd"
  let tString = "e \n f \n ab q \n c \n d"
      scoreSizeJL = fromString tString :: JoinList (Score, Size) String
  print scoreSizeJL

  -- Test fromString . toString
  print tString
  print $ toString scoreSizeJL
  print $ toString scoreSizeJL == tString

  -- Test line
  print $ map (`line` scoreSizeJL) [0..6]

  -- Test replaceLine

  -- Test numLines

  -- Test value


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)


scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine str = Single (scoreString str, Size . length $ lines str) str


instance Buffer (JoinList (Score, Size) String) where
  toString Empty                  = ""
  toString (Single (_,_) st)      = st
  toString (Append (_,_) st1 st2) = toString st1 ++ "\n" ++ toString st2

  fromString "" = Empty
  fromString st = foldr1 (+++) $ map scoreSizeLine (lines st)

  line = indexJ

  replaceLine n l jl = takeJ n jl +++ fromString l +++ dropJ (n+1) jl

  numLines = sz
  value = getScore . fst . tag

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (t1 `mappend` t2) a b
  where
    t1 = tag a
    t2 = tag b


tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


sz :: (Sized m, Monoid m) => JoinList m a -> Int
sz = getSize . size . tag


indexJ :: (Sized b, Monoid b)
        => Int -> JoinList b a -> Maybe a
indexJ _ Empty             = Nothing
indexJ idx a | idx >= sz a = Nothing
             | idx <  0    = Nothing
indexJ 0 (Single _ a)      = Just a
indexJ idx (Append _ a b)  =
  if idx < sz a
    then indexJ idx a
    else indexJ (idx - sz a) b
indexJ _ (Single _ _) = Nothing -- this should not happen with a properly constructed JoinList


dropJ :: (Sized b, Monoid b)
      => Int -> JoinList b a -> JoinList b a
dropJ n jl | n >= sz jl = Empty
dropJ 0 jl              = jl
dropJ n (Append _ a b)  =
  if n < sz a
     then dropJ n a +++ b
     else dropJ (n - sz a) b
dropJ _ _ = Empty -- p Empty/Single for p != 0


takeJ :: (Sized b, Monoid b)
      => Int -> JoinList b a -> JoinList b a
takeJ n jl | n >= sz jl = jl
takeJ 0 _               = Empty
takeJ n (Append _ a b)  =
  if n < sz a
     then takeJ n a
     else a +++ takeJ (n - sz a) b
takeJ _ _ = Empty -- p Empty/Single for p != 0


scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- jlRec :: (Sized b, Monoid b)
--       => c -> () -> Int -> JoinList b a -> c
-- jlRec e ft fe i (Append _ a b) =
--   if i < sz a
--      then jlRec i a -- +++ b
--      else --- a +++ --- jlRec (n - sz a) b


jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:__) !!? 0    = Just x
(_:xs) !!? i    = xs !!? (i-1)


