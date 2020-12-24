-- import Debug.Trace

import Prelude hiding (round)
import qualified Data.IntMap.Strict as M

data Game = Game { round :: Int
                 , word :: Int
                 , history :: M.IntMap Int
                 } deriving (Show, Eq)

main :: IO ()
main = 
  do  let seed = [20, 0, 1, 11, 6, 3]
      -- print seed
      print $ part1 seed
      print $ part2 seed

part1 = word . (gameRound 2020) . seedGame
-- part2 = word . (gameRound 30000000) . seedGame
part2 g0 = (word gf, maximum $ M.keys $ history gf)
  where gf = (gameRound 30000000) $ seedGame g0


seedGame seed = Game {..}
  where round = length seed 
        word = last seed
        history = M.fromList $ zip (init seed) [1..]

infiniteGame g = iterate gameStep g

gameRound r game0 = head $ dropWhile notYet $ infiniteGame game0
  where notYet game = round game < r

gameStep Game{..} = 
  Game { round = round + 1
       , word = word'
       , history = history'
       }
  where 
    word' = speakWord (M.lookup word history) round
    history' = M.insert word round history

speakWord Nothing _ = 0
speakWord (Just prev) now = now - prev


