-- import Debug.Trace

import Prelude hiding (round)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Loops
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as V


main :: IO ()
main = 
  do  let seed = [20, 0, 1, 11, 6, 3]
      -- print seed
      print $ part1 seed
      print $ part2 seed


part1 seed = runGame seed 2020
part2 seed = runGame seed 30000000

runGame seed roundsNeeded =
  runST $ 
    do (round, word, history) <- seedGame seed roundsNeeded
       gameLoop roundsNeeded round word history
       readSTRef word

gameLoop targetRound round word history =
    do gameStep round word history 
         `untilM_` ((== targetRound) <$> readSTRef round)
       return ()

-- gameLoop targetRound round word history =
--     do ( gameStep round word history 
--          `untilM_` (do r <- readSTRef round
--                        return $ r == targetRound)
--         )
--        return ()

-- gameLoop targetRound round word history =
--     do untilM_ (gameStep round word history ) 
--                (do r <- readSTRef round
--                    return $ r == targetRound )
--        return ()

-- gameLoop targetRound round word history =
--     do whileM_ (do r <- readSTRef round
--                    return $ r /= targetRound )
--                (gameStep round word history ) 
--        return ()

-- gameLoop targetRound round word history =
--     do whileM_ (do r <- readSTRef round
--                    return $ r /= targetRound )
--                $ gameStep round word history
--        return ()

-- gameLoop targetRound round word history =
--     do whileM_ ((/= targetRound) <$> readSTRef round)
--                $ gameStep round word history
--        return ()

seedGame seed historySize = 
  do round <- newSTRef $ length seed
     word <- newSTRef $ last seed
     history <- V.replicate historySize 0
     forM_ (zip (init seed) [1..]) $ \(t, s) -> V.write history t s
     return (round, word, history)

gameStep :: STRef s Int -> STRef s Int -> V.MVector s Int -> ST s ()
gameStep round word history =
  do roundVal <- readSTRef round
     wordVal <- readSTRef word
     wordH <- V.read history wordVal
     let word' = speakWord wordH roundVal
     V.write history wordVal roundVal
     modifySTRef round (+1)
     writeSTRef word word'
     return ()

speakWord :: Int -> Int -> Int
speakWord 0 _ = 0
speakWord prev now = now - prev


