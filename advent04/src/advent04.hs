-- import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char
import Data.List

-- import Text.Megaparsec.Debug

type Passport = M.Map String String

requiredFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", 
    "ecl", "pid"]
expectedFields = S.union requiredFields $ S.singleton "cid"


main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent04.txt"
      let passports = successfulParse text
      -- print $ length passports
      putStrLn $ runTests
      print $ part1 passports
      print $ part2 passports

part1 = length . filter hasRequiredFields 
part2 = length . filter validPassport

hasRequiredFields passport = S.null $ requiredFields `S.difference` (M.keysSet passport)

validPassport :: Passport -> Bool
validPassport passport = (hasRequiredFields passport) && (all validField $ M.assocs passport)

validField :: (String, String) -> Bool
validField (key, value) =
  case key of
    "byr" -> validRanged 1920 2002 value
    "iyr" -> validRanged 2010 2020 value
    "eyr" -> validRanged 2020 2030 value
    "hgt" -> validHeight value
    "hcl" -> validHex value
    "ecl" -> validEye value
    "pid" -> validPid value
    "cid" -> True
    _ -> False

validRanged lower upper value = 
  if all isDigit value
  then v >= lower && v <= upper
  else False
  where v = read @Int value 

validHeight value = 
  if "cm" `isSuffixOf` value
  then validRanged 150 193 v
  else if "in" `isSuffixOf` value
       then validRanged 59 76 v
       else False
  where v = reverse $ drop 2 $ reverse value

validHex value = (length value == 7) && (head value == '#') && (all isHexDigit $ tail value)

validEye value = value `S.member` eyeColours
eyeColours = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid value = (length value == 9) && (all isDigit value)

runTests :: String -- [(Text, Bool)]
runTests = if null failures
           then "All tests passed"
           else show failures
  where failures = filter failedTest testCases

failedTest :: (Text, Bool) -> Bool
failedTest (passportText, expected) = actual /= expected
  where passport = parseTestCase passportText
        actual = validPassport passport

parseTestCase :: Text -> Passport
parseTestCase input = 
  case parse passportP "test" input of
    Left  _err -> M.empty
    Right p -> p

testCases = 
  [ ("eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926", False)
  , ("iyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946", False)
  , ("hcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277", False)
  , ("hgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007", False)
  , ("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f", True)
  , ("eyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm", True)
  , ("hcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022", True)
  , ("iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719", True)
  ]


-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome (    char ' ' 
                       <|> char '\t' 
                       <|> (try (newline <* notFollowedBy newline))
                       )
             ) CA.empty CA.empty

blankLines = skipSome newline

symb = L.symbol sc
colonP = symb ":"
hashChar = char '#'
stringP = some (alphaNumChar <|> hashChar) <* sc

kvP = (,) <$> stringP <* colonP <*> stringP

passportsP = passportP `sepBy` blankLines
passportP = M.fromList <$> many kvP

successfulParse :: Text -> [Passport]
successfulParse input = 
  case parse passportsP "input" input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right passports -> passports
