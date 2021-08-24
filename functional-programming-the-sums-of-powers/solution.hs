-- https://www.hackerrank.com/challenges/functional-programming-the-sums-of-powers/problem
import Data.List 

sumsOfPowers :: Int -> Int -> [[Int]]
sumsOfPowers n x = go x possible []
    where
        go target nums sofar
            | target == 0 = [sofar]
            | null nums = []
            | head nums > target = go target (tail nums) sofar
            | otherwise = (go (target - head nums) (tail nums) (head nums: sofar)) ++ (go target (tail nums) sofar)
        possible = reverse $ takeWhile (<=x) ((^n) <$> [1..])

main :: IO ()
main = do
    x <- readInt <$> getLine
    n <- readInt <$> getLine
    print $ length $ sumsOfPowers n x

readInt :: String -> Int
readInt = read
