import Text.Printf (printf)

intervalLength = 0.001

intervalFactory :: Double -> Double -> [Double]
intervalFactory l r = [l + intervalLength * (fromIntegral i) | i <- [0..floor $ (r - l) / intervalLength]]

-- Given a's and b's makes a function for a polynomial of degree (max b).
-- (a1 * x ^ b1) + (a2 * x ^ b2) + ... + (aN * x ^ bN)
fFactory :: [Int] -> [Int] -> Double -> Double
fFactory a b = f
    where
        doubleA = map fromIntegral a :: [Double]
        doubleB = map fromIntegral b :: [Double]
        partA = zipWith (*) doubleA :: [Double] -> [Double]
        partB = (flip map) doubleB . (**) :: Double -> [Double]
        f = sum . partA . partB :: Double -> Double

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [area, volume]
    where
        f = fFactory a b
        xs = intervalFactory (fromIntegral l) (fromIntegral r)
        ys = map f xs
        area = sum $ map (*intervalLength) ys
        volume = sum $ map (\r -> pi * r ** 2 * intervalLength) ys

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
