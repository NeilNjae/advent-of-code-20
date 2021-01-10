-- import Debug.Trace

-- import Math.NumberTheory.Moduli.DiscreteLogarithm
-- import Data.Finite (Finite, modulo, getFinite)
-- import GHC.TypeNats (KnownNat)
-- import Data.Maybe

import Data.Semigroup

newtype MExp = MExp Integer
  deriving (Show, Eq)

instance Semigroup MExp where
  (MExp a) <> (MExp b) = MExp $ (a * b) `mod` modularBase


subject :: MExp
subject = MExp 7

modularBase :: Integer
modularBase = 20201227

main :: IO ()
main = 
    do text <- readFile "data/advent25.txt"
       let [cardKey, doorKey] = map read $ lines text
       print $ part1 cardKey doorKey

part1 cardKey doorKey = publicKey
  where cardRounds = findRounds cardKey
        MExp publicKey = stimes cardRounds (MExp doorKey)

findRounds :: Integer -> Integer
findRounds target = fst $ until (foundRounds t) countingExponential (0, seed)
  where t = MExp target
        seed = MExp 1

foundRounds :: MExp -> (Integer, MExp) -> Bool
foundRounds target (_n, v) = v == target

countingExponential :: (Integer, MExp) -> (Integer, MExp)
countingExponential (n, v) = (n + 1, v <> subject)

