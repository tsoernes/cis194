{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

main :: IO ()
main = do
  -- evalRandIO
  print "HEY"

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

-- Number of units in army
type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  -- Roll dice for attacker and defender
  atkDice <- rollN $ nAttackers $ attackers bf
  defDice <- rollN $ nDefenders $ defenders bf
  -- Sort dice in decending order
  let atkDiceS = sortBy (flip compare) atkDice
      defDiceS = sortBy (flip compare) defDice
      -- Higher die kills a unit in opposing army. A tie kills an attacking unit
      comp = zipWith (>) atkDiceS defDiceS
      defCasualties = length $ filter id comp
      atkCasualties = length comp - defCasualties
  return $ Battlefield (attackers bf - atkCasualties) (defenders bf - defCasualties)

rollN :: Int -> Rand StdGen [DieValue]
rollN n = replicateM n die

nAttackers :: Army -> Int
nAttackers army = min 3 (army - 1)

nDefenders :: Army -> Int
nDefenders = min 2

-- Battle until there are no defenders remaining or fewer than two attackers
invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | defenders bf <= 0 || attackers bf < 2 = return bf
  | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  let n = 1000
  battles <- replicateM n $ invade bf
  let nSuccesses = length $ filter (\bf' -> defenders bf' == 0) battles
  return $ fromIntegral nSuccesses / fromIntegral n


