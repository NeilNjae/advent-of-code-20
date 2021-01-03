-- import Debug.Trace

-- import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Attoparsec.Text hiding (take)
-- import Data.Attoparsec.Combinator
-- import Control.Applicative
-- import Control.Applicative.Combinators

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List
import Control.Monad.CSP


type Ingredient = String
type Allergen = String
data Food = Food 
  { ingredients :: S.Set Ingredient
  , allergens :: S.Set Allergen
  } deriving (Show, Eq)

type CandidateIngredients = M.Map Allergen (S.Set Ingredient)



main :: IO ()
main = 
  do  text <- TIO.readFile "data/advent21.txt"
      let foods = successfulParse text
      let candidates = M.unionsWith S.intersection $ map allergenMap foods
      -- print candidates
      print $ part1 candidates foods
      putStrLn $ part2 candidates


part1 candidates foods = sum $ map countSafe foods
  where possibleAllergens = S.unions $ M.elems candidates
        countSafe food = S.size $ (ingredients food) `S.difference` possibleAllergens

part2 candidates = intercalate "," $ map snd $ sortOn fst assignments
  where assignments = knownAllergens candidates


allergenMap :: Food -> CandidateIngredients
allergenMap food = M.fromList $ S.toList $ S.map (, ingredients food) $ allergens food

knownAllergens :: CandidateIngredients -> [(Allergen, Ingredient)]
knownAllergens candidates = zip allergens assignedIngredients 
  where 
    (allergens, possibleIngredients) = unzip $ M.toList candidates
    assignedIngredients = solveAllergens $ map S.toList possibleIngredients

solveAllergens :: [[Ingredient]] -> [Ingredient]
solveAllergens possibleIngredients = oneCSPSolution $ do
  dvs <- mapM mkDV possibleIngredients
  mapAllPairsM_ (constraint2 (/=)) dvs
  return dvs

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ _f []     = return ()
mapAllPairsM_ _f (_:[]) = return ()
mapAllPairsM_  f (a:l)  = mapM_ (f a) l >> mapAllPairsM_ f l


-- Parse the input file

foodsP = foodP `sepBy` endOfLine
foodP = Food <$> ingredientsP <* " (contains " <*> allergensP <* ")"

ingredientsP = S.fromList <$> (many1 letter) `sepBy` (many1 space)
allergensP = S.fromList <$> (many1 letter) `sepBy` (string ", ")


-- successfulParse :: Text -> (Integer, [Maybe Integer])
successfulParse input = 
  case parseOnly foodsP input of
    Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
    Right foods -> foods
