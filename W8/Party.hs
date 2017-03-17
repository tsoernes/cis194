{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Data.List

import Employee


main :: IO ()
main = do
  file <- readFile "company.txt"
  let company = parseCompany file
      (GL guests fun) = maxFun company
      sortedNames = sort $ map empName guests
  putStrLn $ "Total fun: " ++ show fun
  mapM_ putStrLn sortedNames


parseCompany :: String -> Tree Employee
parseCompany = read


glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)


instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)


moreFun :: GuestList -> GuestList -> GuestList
--moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 >= f2 then gl1 else gl2
moreFun = max


treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root subTrees) = f root $ map (treeFold f) subTrees


--combineGLs :: Employee ->  [GuestList] -> GuestList
--combineGLs e gls = undefined


-- Takes a boss and a tuple for each subtree under the boss.
-- The first GL in the pair is the best GL with the boss included, and the second
-- is the best GL without the boss included.  Returns
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss, woBoss)
  where
    withBoss = glCons boss $ foldMap snd gls
    woBoss   = foldMap fst gls


maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun withBoss woBoss
  where
    (withBoss, woBoss) = treeFold nextLevel tree



