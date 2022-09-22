import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import Data.List (sort)
import Data.Maybe (isJust, fromJust, catMaybes)

type Case = (Int, [Int])
type Result = [Int]

-- returns the count of each element and the order of appearance
countOccurences :: Ord a => [a] -> (Map.Map a Int, [a])
countOccurences l = go l Map.empty []
    where
        go [] m o = (m, reverse o)
        go (x: xs) m o = go xs newm newo
            where
                newm = Map.insertWith (+) x 1 m
                newo = if Map.member x m then o else x:o

work :: Case -> Result
work (k, nums) = if null result then [-1] else result
    where
        (occ, ord) = countOccurences nums
        filtered = Map.filter (>= k) occ
        lu e = e <$ Map.lookup e filtered
        result = catMaybes (lu <$> ord)

work2 :: Case -> Result
work2 (k, nums) = if null result then [-1] else result
    where
        go :: (Ord a, Eq a) => [a] -> Map.Map a Int -> [a]
        go [] m = []
        go (x:xs) m = if count == k then x : go xs new_map else go xs new_map
            where
                count = 1 + Map.findWithDefault 0 x m
                new_map = Map.insert x count m
        result = go nums Map.empty

main = do
    t <- readInt <$> getLine
    cases <- replicateM t getCase
    mapM_ (printResult . work) cases

printResult :: Result -> IO ()
printResult = putStrLn . unwords . fmap show

readInt = read :: String -> Int

readIntList = fmap readInt . words

getCase = do
    (n: k: _) <- readIntList <$> getLine
    nums <- readIntList <$> getLine
    return (k, nums)
