-- https://www.hackerrank.com/challenges/crush/
-- Start: 24/08/2021
module Main where

import Data.Maybe (fromMaybe)
import Data.Function (on)

import qualified Data.Map.Strict as Map

type Query = [Int]

type Tally = Map.Map Int (Int, Int)

merge :: Tally -> Query -> Tally
merge m q = if Map.null m
    then
        Map.singleton a0 (b0, x0)
    else
        Map.fromList $ go [a0,b0,x0] $ Map.toAscList m
    where
        [a0, b0, x0] = take 3 q 
        go [a,b,x] [] = (a,(b,x)) : []
        go [a,b,x] l@( (c,(d,y)) : t) =
            if a < c
                then
                    if b < c
                        then
                            --  a     b
                            --  |-----|
                            --           |------|
                            --           c      d
                            (a,(b,x)) : l
                        else -- a < c  && b >= c

                            --  a      b
                            --  |------| 
                            --     |---------|
                            --     c         d
                            -- Result: Add (a,c-1,x); Merge (c,b,x)
                            (a,(c-1,x)) : (go [c,b,x] l)
                else
                    if a == c
                        then
                            if b == d
                                then
                                    (a,(b,x+y)) : t
                                else
                                    if b <  d
                                        then
                                            (a,(b,x+y)): (b+1,(d,y)): t
                                        else -- d < b && a == c
                                            --  a        b
                                            --  |--------|
                                            --  |----|
                                            --  c    d
                                            (c,(d,x+y)) : (go [d+1,b,x] t)
                        else -- a > c
                            if a > d
                                then
                                    --            a     b
                                    --            |-----|
                                    --  c      d
                                    --  |------|
                                    (c,(d,y)) : go [a,b,x] t
                                else -- a > c && a <= d
                                            (c,(a-1,y)) : (a,(d,x+y)) : (go [d+1,b,x] t)
                                        --      a       b               a    b
                                        --      |-------|    OR         |----|
                                        --  |--------|             |----|
                                        --  c        d             c    d

ensure :: (a->Bool) -> a -> Maybe a
ensure p v = if p v then Just v else Nothing

crush :: [Query] -> Int
crush qs = fromMaybe 0 $ maxtally
    where
        maxtally = fmap maximum $ ensure (not.null) $ values
        values = (snd . snd) <$> (Map.toList m)
        m = foldl merge Map.empty qs

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
