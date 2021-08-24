-- https://www.hackerrank.com/challenges/convex-hull-fp/problem
import Data.List
import Data.Function
import qualified Data.Set as S

type Shape = [Point]
type Point = (Int, Int)
data Relation = Colinear | LeftTurn | RightTurn deriving (Show)

vecMinus :: Point -> Point -> Point
vecMinus (x1, y1) (x2, y2) = (x1-x2, y1-y2)
vecTan :: Point -> Double
vecTan (x, y) = atan2 (fromIntegral y) (fromIntegral x)

distance :: (Point, Point) -> Double
distance ((x1, y1), (x2, y2)) = sqrt $ (fromIntegral $ x2 - x1) ** 2 + (fromIntegral $ y2 - y1) ** 2

perimeter :: Shape -> Double
perimeter points =
    sum sideLengths
    where
        pointPairs = consecutivePairsLoop points
        sideLengths = map distance pointPairs

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs (x:rest@(y:_)) = (x, y) : (consecutivePairs rest)
consecutivePairs _ = []

-- Assumes list has at least 3 items otherwise the loop (last) pair will not
-- make sense
consecutivePairsLoop :: [a] -> [(a, a)]
consecutivePairsLoop l = consecutivePairs l ++ [(last l, head l)]


compareAngles :: Point -> Point -> Ordering
compareAngles = compare `on` vecTan

-- Normalises the last two points by subtracting the first point (the 'origin')
normAndCompare :: Point -> Point -> Point -> Ordering
normAndCompare z = compareAngles `on` (`vecMinus` z)

convexHull :: [Point] -> Shape
convexHull s = go remaining_points [start_point]
    where
        -- Remove duplicates
        all_points = S.toList (S.fromList s :: S.Set Point)

        compareY (x1, y1) (x2, y2) = if r1 == EQ then r2 else r1
            where
                r1 = compare y1 y2
                r2 = compare x1 x2

        start_point = head $ sortBy compareY all_points
        remaining_points = sortBy (normAndCompare start_point) (delete start_point all_points)

        go [] result = result
        go rems result = if length result > 1
            then
                case (let top:second_top:_ = result in relation second_top top (head rems)) of
                    LeftTurn -> go (tail rems) (head rems:result)
                    _ -> go rems (tail result)
            else
                go (tail rems) (head rems:result)


-- https://en.wikipedia.org/wiki/Graham_scan
-- 
-- If the result is 0, the points are collinear; if it is positive, the three 
-- points constitute a "left turn" or counter-clockwise orientation, otherwise
-- a "right turn" or clockwise orientation (for counter-clockwise numbered
-- points). 
relation :: (Ord a, Num a) => (a, a) -> (a, a) -> (a, a) -> Relation
relation (x1, y1) (x2, y2) (x3, y3) = go $ (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
    where
        go r
            | r > 0 = LeftTurn -- Counter-clockwise
            | r == 0 = Colinear
            | otherwise = RightTurn -- Clockwise rotation

main :: IO ()
main = do
    n <- readLn :: IO Int
    numbers <- sequence $ replicate n readNumsLn
    let points = [(x,y) | (x:y:_) <- numbers]
    print $ perimeter $ convexHull points

readNumsLn :: IO [Int]
readNumsLn =  (map read) . words <$> getLine