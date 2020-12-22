-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text
-- import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Maybe
import Data.List
import Math.NumberTheory.Moduli.Chinese



main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent13.txt"
      let (timestamp, busses) = successfulParse text
      print timestamp
      print busses
      print $ part1 timestamp busses
      print $ part2 busses

part1 timestamp maybeBusses = eBus * eTime
  where busses = catMaybes maybeBusses
        busDepartures = map (busAndTime timestamp) busses
        (eBus, eTime) = head $ sortOn snd busDepartures

part2 maybeBusses = b - a
  where (a, b) = earliestGroup $ periodOffsets maybeBusses


busAndTime timestamp period = (period, earliestDeparture timestamp period)

earliestDeparture timestamp period = period - (timestamp `mod` period)

periodOffsets :: [Maybe Integer] -> [(Integer, Integer)]
periodOffsets maybeBusses = offsetBusses
  where offsetMaybeBusses = zip [0..] maybeBusses
        offsetJustBusses = filter (isJust . snd) offsetMaybeBusses
        offsetBusses = map (\(o, b) -> (o, fromJust b)) offsetJustBusses


earliestGroup :: [(Integer, Integer)] -> (Integer, Integer)
earliestGroup offsetBusses = foldl1 chineseStep offsetBusses


chineseStep (n1, m1) (n2, m2) = (n, m1 * m2)
  where n = fromJust $ chinese (n1, m1) (n2, m2)


-- Parse the input file

timeBusP = (,) <$> decimal <* endOfLine <*> busPeriodsP

busPeriodsP = busPeriodP `sepBy` (string ",")

busPeriodP = (Just <$> decimal) <|> ("x" *> pure Nothing)


successfulParse :: Text -> (Integer, [Maybe Integer])
successfulParse input = 
  case parseOnly timeBusP input of
    Left  _err -> (0, []) -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right timeBus -> timeBus
