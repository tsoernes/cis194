{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char


newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)


instance Monoid Score where
  mempty = Score 0
  mappend = (+)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score c | uc `elem` "EAIONRTLSU" = 1
        | uc `elem` "DG"         = 2
        | uc `elem` "BCMP"       = 3
        | uc `elem` "FHVWY"      = 4
        | uc  ==    'K'          = 5
        | uc `elem` "JX"         = 8
        | uc `elem` "QZ"         = 10
        | otherwise              = 0
  where uc = toUpper c

scoreString :: String -> Score
scoreString = sum . map score
