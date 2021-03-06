-- import Debug.Trace

import Prelude hiding (round)
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as V


type STInt s = STRef s Int
type VInt s = V.MVector s Int

main :: IO ()
main = 
  do  let seed = [20, 0, 1, 11, 6, 3]
      -- print seed
      print $ part1 seed
      print $ part2 seed


part1 seed = runGame seed 2020
part2 seed = runGame seed 30000000

runGame :: [Int] -> Int -> Int
runGame seed roundsNeeded =
  runST $ 
    do (round, word, history) <- seedGame seed roundsNeeded
       gameSteps roundsNeeded round word history
       readSTRef word

seedGame :: [Int] -> Int -> ST s (STInt s, STInt s,  VInt s)
seedGame seed historySize = 
  do round <- newSTRef $ length seed
     word <- newSTRef $ last seed
     history <- V.replicate historySize 0
     forM_ (zip (init seed) [1..]) $ \(t, s) -> V.write history t s
     return (round, word, history)


-- gameSteps :: Int -> STRef s Int -> STRef s Int -> V.MVector s Int -> ST s ()
gameSteps :: Int -> STInt s -> STInt s -> VInt s -> ST s ()
gameSteps targetRound round word history =
  do roundVal <- readSTRef round
     if roundVal == targetRound
     then return ()
     else do 
           wordVal <- readSTRef word
           wordH <- V.read history wordVal
           let word' = speakWord wordH roundVal
           V.write history wordVal roundVal
           modifySTRef round (+1)
           writeSTRef word word'
           gameSteps targetRound round word history


speakWord :: Int -> Int -> Int
speakWord 0 _ = 0
speakWord prev now = now - prev


