import Control.Monad (replicateM)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type Factorized = [(Int, Int)]

commonKeys [] = Set.empty
commonKeys (m : ms) = foldl Set.intersection (Map.keysSet m) (Map.keysSet <$> ms)

gcdFactorized :: [Factorized] -> Factorized
gcdFactorized numbers = sort result
  where
    maps = Map.fromList <$> numbers
    mapKeysSet = Map.keysSet <$> maps
    common = commonKeys maps
    result = [(k, minimum (fromJust . Map.lookup k <$> maps)) | k <- Set.toList common]

pairs (x : y : rest) = (x, y) : pairs rest
pairs _ = []

unpairs [] = []
unpairs ((x, y) : rest) = x : y : unpairs rest

main = do
  n <- readInt <$> getLine
  numbers <- fmap (pairs . readIntList) <$> replicateM n getLine
  putStrLn $ showFac $ gcdFactorized numbers

readInt = read :: String -> Int

readIntList = fmap readInt . words

showFac :: Factorized -> String
showFac = unwords . fmap show . unpairs
