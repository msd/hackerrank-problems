-- https://www.hackerrank.com/challenges/crush/
-- Start: 24/08/2021
module Solution where

import Data.Maybe (fromMaybe)
import Data.Function (on)

import qualified Data.Map.Strict as Map

type Query = [Int]

type Tally = Map.Map (Int, Int) Int

(.-) :: (c->d) -> (a->b->c) -> a -> b -> d
(.-) f1 f2 x y = f1 (f2 x y)

merge :: Tally -> Tally -> Tally
merge = (go `on` Map.toAscList)
    where
        go [] m = Map.fromList m
        go m [] = Map.fromList m
        go (h1:t1) (h2:t2) = (Map.fromList $ goHead h1 h2) `Map.union` (go t1 t2)
        goHead q1@((a,b),x) q2@((c,d),y) = if a < c
            then 
                if b < c
                    then
                        [q1, q2]
                    else if b < d
                        then
                            [((a,c-1),x), ((c,b),x+y), ((b+1,d),y)]
                        else if b == d
                            then
                                [((a,c-1),x), ((c,d),x+y)]
                            else -- d < b
                                [((a,c-1),x), ((c,d),x+y), ((d+1,b),x)]
            else if a == c
                then if b < d
                    then
                        [((a,b),x+y), ((b+1,d),y)]
                    else if b == d
                        then
                            [((a,b),x+y)]
                        else -- d < b
                            [((a,d),x+y), ((d+1,b),x)]
                else -- c < a
                    if d < a
                        then
                            [q1, q2]
                        else if b < d
                            then
                                [((c,a-1),y), ((a,b),x+y), ((b+1,d),y)]
                            else if b == d
                                then
                                    [((c,a-1),y), ((a,b),x+y)]
                                else -- d < b
                                    [((c,a-1),y), ((a,d),x+y), ((d+1,b),x)]

ensure :: (a->Bool) -> a -> Maybe a
ensure p v = if p v then Just v else Nothing

findMaxValue :: Ord b => Map.Map a b -> Maybe b
findMaxValue = (maximum <$>) . (ensure (not . null)) . Map.elems

-- findMaxValuePairs :: Ord b => Map.Map a b -> [(a,b)]
-- findMaxValuePairs m = maybe []  go maxv 
--     where
--         (<#>) = (,)
--         go v = (<#> v) <$> (filterkeys v)
--         filterkeys v = Map.keys $ Map.filter (==v) m
--         maxv = findMaxValue m

crush :: [Query] -> Int
crush qs = fromMaybe 0 maxv
    where
        maxv = findMaxValue m
        m = foldl merge Map.empty (singletonQuery <$> qs)
        singletonQuery [a,b,k] = Map.singleton (a,b) k

main :: IO ()
main = do
    [n, m] <- (take 2) . readInts <$> getLine
    let readquery = (take 3) . readInts <$> getLine
    queries <- sequence (replicate m readquery)
    print $ crush queries

readInt :: String -> Int
readInt = read

readInts :: String -> [Int]
readInts = fmap readInt . words
