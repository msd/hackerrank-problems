-- import qualified Data.Set as S
import qualified Data.List as L

readPair :: IO (Int, Int)
readPair = do
    twolist <- getLine >>= (return . (map (read :: String -> Int)) . words)
    return (twolist !! 0, twolist !! 1)

readPairs :: Int -> IO [(Int, Int)]
readPairs 0 = do return []
readPairs n = do
    thehead <- readPair
    thetail <- readPairs (n - 1)
    return (thehead:thetail)

runCase :: IO Bool
runCase = do
    n <- readLn :: IO Int
    pairs <- readPairs n
    return (isFunction pairs)

runCases :: Int -> IO ()
runCases 0 = return ()
runCases t = do
    r <- runCase
    putStrLn (if r then "YES" else "NO")
    runCases (t - 1)

main :: IO ()
main = do
    t <- readLn :: IO Int
    runCases t

isFunction :: [(Int, Int)] -> Bool
isFunction ps =
    length xs == (length $ L.nub xs)
    where 
        xs = [x | (x, _) <- ps]
        ys = [y | (_, y) <- ps]