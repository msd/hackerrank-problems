import Data.List (partition, sort, (\\))
import qualified Data.Set as Set
import Data.Function (on)

countTriplets :: Int -> [Int] -> Int
countTriplets r l = go $ sort l
    where
        go :: [Int] -> Int
        go nums = if null nums
            then
                0
            else
                (product (countToThree prog)) + (go nums')
                where
                    nums_head = head nums
                    prog = takeProgression r nums_head nums
                    nums' = dropWhile (==nums_head) nums

-- Takes a list of zeros ones and twos and returns a list 
-- [c0, c1, c2] the count of each respectively
countToThree :: [Int] -> [Int]
countToThree [] = [0,0,0]
countToThree (h:t) = case h of
        0 -> [x+1,y,z]
        1 -> [x,y+1,z]
        2 -> [x,y,z+1]
    where
        [x,y,z] = countToThree t

-- Take all from list l that are equal to i, i*r or i*r*r
-- and returns a list of 0, 1 and 2 respectively for each occurence
takeProgression :: Int -> Int -> [Int] -> [Int]
takeProgression r i l = unpow <$> fil
    where
        unpow :: Int -> Int
        unpow = round . (logBase (fromIntegral r)) . (/ fromIntegral i) . fromIntegral
        mypowers = take 3 $ (i*) <$> (powersOne r)
        fil = filter (`elem` mypowers) l

powers :: Num a => a -> [a]
powers x = iterate (*x) x

powersOne = (1:) . powers

-- https://wetmore.github.io/posts/n-choose-k-the-haskell-way.html
-- instance Num a => Num [a] where
--     fromInteger n = [fromInteger n]
--     (x:xs) + (y:ys) = (x + y) : (xs + ys)
--     xs + [] = xs
--     [] + ys = ys
--     (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
--     _ * _ = []
-- pascal :: [[Int]]
-- pascal = powers [1,1]
-- choose :: Int -> Int -> Int
-- choose n k = pascal !! (n - 1) !! k

main :: IO ()
main = do
    l0 <- getLine
    l1 <- getLine
    let [n, r] = take 2 $ readInts l0
    let arr = take n $ readInts l1
    print $ countTriplets r arr

readInt :: String -> Int
readInt = read

readInts :: String -> [Int]
readInts = (readInt <$>) . words
