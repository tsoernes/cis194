module W4 () where

import Test.QuickCheck


fun1 :: [Integer] -> Integer
fun1 xs = foldr (\y ys -> (y-2) * ys) 1 (filter even xs)

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
  | even x    = (x-2) * fun1' xs
  | otherwise = fun1' xs

prop_fun1 xs = fun1 xs == fun1' xs


fun2 :: Integer -> Integer
fun2 n = sum . filter even . takeWhile (>1) $ iterate (\x -> if even x then x `div` 2 else 3*x+1) n

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n | even n    = n + fun2' (n `div` 2)
        | otherwise = fun2' (3 * n + 1)

prop_fun2 x = fun2 x == fun2' x


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

depth :: Tree a -> Integer
depth Leaf           = -1
depth (Node k _ _ _) = k

-- | Take a list of elements and construct a balanced tree
foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

-- | Insert an element into a tree, keeping it balanced
treeInsert :: a -> Tree a -> Tree a
treeInsert a Leaf = Node 0 Leaf a Leaf
treeInsert a (Node i t1 b t2)
  | depth t1 < depth t2 = Node (depth t2+1) (treeInsert a t1) b t2 -- Unequally deep children means inserting
  | depth t2 < depth t1 = Node (depth t1+1) t1 b (treeInsert a t2) --   can be done without increasing depth
  | otherwise           = Node (j+1) t3 b t2 -- Equally deep children means we don't know if inserting will increase depth
  where
    t3@(Node j _ _ _) = treeInsert a t1

-- Verify that a tree is balanced
prop_foldTreeBal :: Tree a -> Bool
prop_foldTreeBal Leaf = True
prop_foldTreeBal (Node _ t1 _ t2) = (abs (depth t1 - depth t2) <= 1)
  && prop_foldTreeBal t1 && prop_foldTreeBal t2

prop_foldTreeDepth :: Tree a -> Bool
prop_foldTreeDepth Leaf = True
prop_foldTreeDepth (Node i t1 _ t2) = i-1 == max (depth t1) (depth t2)
  && prop_foldTreeDepth t1 && prop_foldTreeDepth t2


xor :: [Bool] -> Bool
xor = foldr (/=) False

prop_xor :: [Bool] -> Bool
prop_xor bs = xor bs == odd (length (filter (==True) bs))


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

prop_map f xs = map' f xs == map f xs

-- Generate all odd prime number up to 2n+2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x+1 | x <- [1..n], x `notElem` [i+j+2*i*j | j <- [1..n], i <- [1..j], i+j+2*i*j <= n]]




