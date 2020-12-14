-- import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text
-- import Data.Attoparsec.Combinator
import Control.Applicative

import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Vector ((!), (//))

data Instruction = Acc Int | Jmp Int | Nop Int
  deriving (Show, Eq)

type Program = V.Vector Instruction

data Machine = Machine 
  { machineProgram :: Program
  , machineIP :: Int
  , machineAcc :: Int
  }

data MachineResult = Looped Int | Terminated Int | OutOfBounds Int Int
  deriving (Show, Eq)


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent08.txt"
      let program = successfulParse text
      -- print program
      let machine = Machine program 0 0
      print $ part1 machine
      print $ part2 program

part1 machine = executeMany S.empty machine

part2 program = filter terminates $ map runProgram programs
  where programs = altPrograms program
        runProgram p = executeMany S.empty (Machine p 0 0)
        terminates (Terminated _) = True
        terminates (OutOfBounds _ _) = True
        terminates _ = False


executeMany visited machine
  | currentIP `S.member` visited = Looped (machineAcc machine)
  | currentIP == programSize     = Terminated (machineAcc machine)
  | currentIP > programSize      = OutOfBounds (machineAcc machine) currentIP
  | otherwise                    = executeMany visited' machine'
  where machine' = executeStep machine
        currentIP = machineIP machine
        visited' = S.insert currentIP visited
        programSize = V.length $ machineProgram machine


executeStep m = execute ((machineProgram m)!(machineIP m)) m

execute (Acc n) m = m { machineIP = machineIP m + 1
                      , machineAcc = machineAcc m + n
                      }
execute (Jmp n) m = m { machineIP = machineIP m + n }
execute (Nop n) m = m { machineIP = machineIP m + 1 }

 
altPrograms program = map (mutateProgram program) [0..(V.length program - 1)]

mutateProgram program i = go (program!i)
  where go (Nop n) = program // [(i, Jmp n)]
        go (Jmp n) = program // [(i, Nop n)]
        go _ = program

-- -- Parse the input file


instructionP = accP <|> jmpP <|> nopP

accP = Acc <$> ("acc " *> signed decimal)
jmpP = Jmp <$> ("jmp " *> signed decimal)
nopP = Nop <$> ("nop " *> signed decimal)

programP = V.fromList <$> sepBy instructionP endOfLine

successfulParse :: Text -> Program
successfulParse input = 
  case parseOnly programP input of
    Left  _err -> V.empty -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right program -> program
