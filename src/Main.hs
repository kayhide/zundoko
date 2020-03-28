module Main where

import ClassyPrelude


import Data.List (inits)
import Data.Maybe (fromJust)
import System.Random (Random (..), getStdGen)

data Verse = Zun | Doko deriving (Eq, Enum, Bounded)
data Coda = Kiyoshi

instance Show Verse where
  show Zun  = "ずん"
  show Doko = "どこ"
  -- show Zun = "Zoon"
  -- show Doko = "Doko"

instance Show Coda where
  show Kiyoshi = "きよし"
  -- show Kiyoshi = "Kiyoshi"

instance Random Verse where
  random g = randomR (minBound :: Verse, maxBound :: Verse) g
  randomR (a, b) g = (toEnum r, g')
    where (r, g') = randomR (fromEnum a, fromEnum b) g

isMatched :: [Verse] -> Bool
isMatched = isSuffixOf [Zun, Zun, Zun, Zun, Doko]

main :: IO ()
main = do
  gen <- getStdGen
  traverse_ print $ fromJust $ (find isMatched . inits) $ randoms gen
  print Kiyoshi
