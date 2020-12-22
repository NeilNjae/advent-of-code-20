-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text hiding (take)
-- import Data.Attoparsec.Combinator
import Control.Applicative

import Data.Int
import Data.Bits
import Data.List
import qualified Data.Map.Strict as M


data MaskValue = Zero | One | Wild deriving (Show, Eq)
type MaskMap = M.Map Int MaskValue
data Instruction = Mask MaskMap | Assignment Int64 Int64
  deriving (Show, Eq)

type Memory = M.Map Int64 Int64
data Machine = Machine { mMemory :: Memory
                       , mMask :: MaskMap
                       , mMask0 :: Int64
                       , mMask1 :: Int64
                       } deriving (Show, Eq)

emtpyMachine = Machine M.empty M.empty (complement 0) 0


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent14.txt"
      let program = successfulParse text
      print $ part1 program
      print $ part2 program


part1 program = sum $ M.elems $ mMemory finalMachine
  where finalMachine = executeInstructions1 program

part2 program = sum $ M.elems $ mMemory finalMachine
  where finalMachine = executeInstructions2 program

executeInstructions1 instructions = 
  foldl' executeInstruction1 emtpyMachine instructions

executeInstruction1 machine (Mask mask) = makeMask machine mask
executeInstruction1 machine (Assignment loc value) = 
  assignValue machine loc value

makeMask machine mask = 
  machine {mMask0 = maskZeroes mask, mMask1 = maskOnes mask}

assignValue machine loc value = 
  machine {mMemory = M.insert loc value' mem}
  where value' = maskValue machine value
        mem = mMemory machine

maskValue machine value = 
  (value .|. (mMask1 machine)) .&. (mMask0 machine)

maskOnes :: MaskMap -> Int64
maskOnes mask = foldl' setBit zeroBits ones
  where ones = M.keys $ M.filter (== One) mask

maskZeroes :: MaskMap -> Int64
maskZeroes mask = complement $ foldl' setBit zeroBits ones
  where ones = M.keys $ M.filter (== Zero) mask


executeInstructions2 instructions = 
  foldl' executeInstruction2 emtpyMachine instructions

executeInstruction2 machine (Mask mask) = machine {mMask = mask}
executeInstruction2 machine (Assignment loc value) = machine {mMemory = mem'}
  where locs = map encodeMask $ applyAddressMask (mMask machine) $ decodeMask loc
        mem = mMemory machine
        mem' = foldl' (\m l -> M.insert l value m) mem locs


encodeMask :: MaskMap -> Int64
encodeMask mask = M.foldrWithKey' setBitValue zeroBits mask
  where setBitValue _ Zero n = n
        setBitValue i One n = setBit n i

decodeMask :: Int64 -> MaskMap
decodeMask val = M.fromList [ (i, decodeBit $ testBit val i) 
                            | i <- [0..(finiteBitSize val)] 
                            ]
  where decodeBit True = One
        decodeBit False = Zero

applyAddressMask :: MaskMap -> MaskMap -> [MaskMap]
applyAddressMask mask address = M.foldrWithKey' applyBit [address] mask

applyBit :: Int -> MaskValue -> [MaskMap] -> [MaskMap]
applyBit _ Zero ms = ms
applyBit k One  ms = [ M.insert k One m | m <- ms ]
applyBit k Wild ms = [ M.insert k b m | m <- ms, b <- [Zero, One] ]

-- Parse the input file

programP = (maskP <|> assignmentP) `sepBy` endOfLine

maskP = maskify <$> ("mask = " *> (many (digit <|> letter)))
assignmentP = Assignment <$> ("mem[" *> decimal) <* "] = " <*> decimal

maskify :: String -> Instruction
maskify chars = Mask (M.fromList locValues)
  where mValues = map readMaskChar chars
        locValues = zip [0..] $ reverse mValues

readMaskChar '0' = Zero
readMaskChar '1' = One
readMaskChar 'X' = Wild

-- successfulParse :: Text -> (Integer, [Maybe Integer])
successfulParse input = 
  case parseOnly programP input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right program -> program
