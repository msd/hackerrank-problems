readPair :: IO (Int, Int)
readPair = do
    twolist <- getLine >>= (return . (map (read :: String -> Int)) . words)
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
    print (areaPolygon pairs)

type Point = (Int, Int)
type Polygon = [Point]

-- pointDiff :: Point -> Point -> Point
-- pointDiff (x1, y1) (x2, y2) = ((x2 - x1), (y2 - y1))

areaPolygonSide :: (Point, Point) -> Double
areaPolygonSide ((x1, y1), (x2, y2)) = w * h
    where
        w = (fromIntegral $ x2 + x1) / 2 :: Double
        h = (fromIntegral $ y2 - y1) :: Double

areaPolygon :: Polygon -> Double
areaPolygon p = 
    sum sideAreas
    where
        pointPairs = consecutivePairsLoop p
        sideAreas = map areaPolygonSide pointPairs

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs (x:rest@(y:z)) = (x, y) : (consecutivePairs rest)
consecutivePairs _ = []

-- Assumes list has at least 3 items otherwise the loop (last) pair will not
-- make sense
consecutivePairsLoop :: [a] -> [(a, a)]
consecutivePairsLoop l = consecutivePairs l ++ [(last l, head l)]
