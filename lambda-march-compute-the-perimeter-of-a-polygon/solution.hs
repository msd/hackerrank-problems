readPair :: IO (Int, Int)
readPair = do
    twolist <- (map (read :: String -> Int)) . words <$> getLine
    True <- return (length twolist >= 2)
    return (twolist !! 0, twolist !! 1)

readPairs :: Int -> IO [(Int, Int)]
readPairs 0 = do return []
readPairs n = do
    thehead <- readPair
    thetail <- readPairs (n - 1)
    return (thehead:thetail)
    
main :: IO ()
main = do
    n <- readLn :: IO Int
    pairs <- readPairs n
    print (perimeter pairs)

type Point = (Int, Int)

distance :: (Point, Point) -> Double
distance ((x1, y1), (x2, y2)) = sqrt $ (fromIntegral $ x2 - x1) ** 2 + (fromIntegral $ y2 - y1) ** 2

perimeter :: [Point] -> Double
perimeter points =
    sum sideLengths
    where
        pointPairs = consecutivePairsLoop points
        sideLengths = map distance pointPairs

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs (x:rest@(y:z)) = (x, y) : (consecutivePairs rest)
consecutivePairs _ = []

-- Assumes list has at least 3 items otherwise the loop (last) pair will not
-- make sense
consecutivePairsLoop :: [a] -> [(a, a)]
consecutivePairsLoop l = consecutivePairs l ++ [(last l, head l)]
