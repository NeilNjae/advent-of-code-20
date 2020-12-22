-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text hiding (take)
-- import Data.Attoparsec.Combinator
import Control.Applicative

import Data.Int
import Data.Bits
import Data.Char
import Data.List

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))


type MaskMap = M.Map Int Int
data Instruction = Mask MaskMap | Assignment Int64 Int64
  deriving (Show, Eq)

type Memory = M.Map Int64 Int64
data Machine = Machine { mMemory :: Memory
                       , mMask0 :: Int64
                       , mMask1 :: Int64
                       } deriving (Show, Eq)

emtpyMachine = Machine M.empty (complement 0) 0


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent14.txt"
      let program = successfulParse text
      print $ take 6 program
      print $ part1 program


part1 program = sum $ M.elems $ mMemory finalMachine
  where finalMachine = executeInstructions program

executeInstructions instructions = 
  foldl' executeInstruction emtpyMachine instructions

executeInstruction machine (Mask mask) = makeMask machine mask
executeInstruction machine (Assignment loc value) = assignValue machine loc value


makeMask machine mask = 
  machine {mMask0 = maskZeroes mask, mMask1 = maskOnes mask}

maskValue machine value = 
  (value .|. (mMask1 machine)) .&. (mMask0 machine)

assignValue machine loc value = 
  machine {mMemory = M.insert loc value' mem}
  where value' = maskValue machine value
        mem = mMemory machine


maskOnes :: MaskMap -> Int64
maskOnes mask = foldl' setBit zeroBits ones
  where ones = M.keys $ M.filter (== 1) mask

maskZeroes :: MaskMap -> Int64
maskZeroes mask = complement $ foldl' setBit zeroBits ones
  where ones = M.keys $ M.filter (== 0) mask


-- Parse the input file

programP = (maskP <|> assignmentP) `sepBy` endOfLine

maskP = maskify <$> ("mask = " *> (many (digit <|> letter)))
assignmentP = Assignment <$> ("mem[" *> decimal) <* "] = " <*> decimal

maskify :: String -> Instruction
maskify chars = Mask (M.fromList locNums)
  where locChars = zip [0..] $ reverse chars
        locDigits = filter (isDigit . snd) locChars
        locNums = map (\(i, n) -> (i, read @Int [n])) locDigits


-- successfulParse :: Text -> (Integer, [Maybe Integer])
successfulParse input = 
  case parseOnly programP input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right program -> program
