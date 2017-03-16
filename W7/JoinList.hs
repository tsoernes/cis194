module JoinList () where

import Data.Monoid

import Sized

main :: IO ()
main = do
  let a = Single (Size 1) "a"
      b = Single (Size 1) "b"
      c = Single (Size 1) "c"
      d = Single (Size 1) "d"
      e = Single (Size 1) "e"
      f = Single (Size 1) "f"

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
  print $ map (`drop` jlToList jli) [0..6]
  print $ map (\x -> jlToList $ dropJ x jli) [0..6]


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

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
indexJ idx a | idx >= sz a = Nothing
indexJ 0 (Single _ a)      = Just a
indexJ idx (Append _ a b)  =
  if idx < sz a
    then indexJ idx a
    else indexJ (idx - sz a) b

dropJ :: (Sized b, Monoid b)
      => Int -> JoinList b a -> JoinList b a
dropJ n jl | n >= sz jl = Empty
dropJ 0 jl = jl
dropJ n (Append _ a b) =
  if n < sz a
     then dropJ n a +++ b
     else dropJ (n - sz a) b

takeJ :: (Sized b, Monoid b)
      => Int -> JoinList b a -> JoinList b a
takeJ n jl | n >= sz jl = jl
takeJ 0 jl = Empty
takeJ n (Append _ a b)  =
  if n < sz a
     then takeJ n a
     else a +++ takeJ (n - sz a) b

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
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)


