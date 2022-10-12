-- https://www.hackerrank.com/challenges/crush/
-- Start: 24/08/2021, DNF
-- Start: 12/10/2022

module Main where

import Control.Monad (replicateM)

type Query = (Int, Int, Int)

-- import Data.Function (on)
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Data.Maybe (fromMaybe)

-- type Tally = Map Int (Int, Int)

-- merge :: Tally -> Query -> Tally
-- merge m q = if Map.null m
--     then
--         Map.singleton a0 (b0, x0)
--     else
--         Map.fromList $ go [a0,b0,x0] $ Map.toAscList m
--     where
--         [a0, b0, x0] = take 3 q
--         go [a,b,x] [] = (a,(b,x)) : []
--         go [a,b,x] l@( (c,(d,y)) : t) =
--             if a < c
--                 then
--                     if b < c
--                         then
--                             --  a     b
--                             --  |-----|
--                             --           |------|
--                             --           c      d
--                             (a,(b,x)) : l
--                         else -- a < c  && b >= c

--                             --  a      b
--                             --  |------|
--                             --     |---------|
--                             --     c         d
--                             -- Result: Add (a,c-1,x); Merge (c,b,x)
--                             (a,(c-1,x)) : (go [c,b,x] l)
--                 else
--                     if a == c
--                         then
--                             if b == d
--                                 then
--                                     (a,(b,x+y)) : t
--                                 else
--                                     if b <  d
--                                         then
--                                             (a,(b,x+y)): (b+1,(d,y)): t
--                                         else -- d < b && a == c
--                                             --  a        b
--                                             --  |--------|
--                                             --  |----|
--                                             --  c    d
--                                             (c,(d,x+y)) : (go [d+1,b,x] t)
--                         else -- a > c
--                             if a > d
--                                 then
--                                     --            a     b
--                                     --            |-----|
--                                     --  c      d
--                                     --  |------|
--                                     (c,(d,y)) : go [a,b,x] t
--                                 else -- a > c && a <= d
--                                             (c,(a-1,y)) : (a,(d,x+y)) : (go [d+1,b,x] t)
--                                         --      a       b               a    b
--                                         --      |-------|    OR         |----|
--                                         --  |--------|             |----|
--                                         --  c        d             c    d

-- crush :: [Query] -> Int
-- crush qs = fromMaybe 0 maxtally
--   where
--     maxtally = maximum <$> ensure (not . null) values
--     values = snd . snd <$> Map.toList m
--     m = foldl merge Map.empty qs

-- ensure :: (a -> Bool) -> a -> Maybe a
-- ensure p v = if p v then Just v else Nothing

-- getOverlap :: Int -> Int -> Tally -> [(Int, (Int, Int))]
-- getOverlap lo hi m = Map.toList $ Map.filterWithKey (\a (b, _val) -> not (a > hi || b < lo)) m

-- -- crush :: [Query] -> Int
-- crush queries = go queries Map.empty
--   where
--     go [] m = 0
--     go (q : qs) m = go qs m2
--       where
--         m2 = m
--         (a, b, i) = q

insertNewQuery :: [Query] -> Query -> [Query]
insertNewQuery [] newQuery = [newQuery]
insertNewQuery (f : fs) newQuery
  | x > b = f : insertNewQuery fs newQuery
  | y < a = newQuery : f : fs
  -- x <= b && y >= a
  | x == a && y < b = (x, y, v + val) : (y + 1, b, v) : fs
  | x == a && y == b = (x, y, v + val) : fs
  | x == a && y > b = (a, b, v + val) : insertNewQuery fs (b + 1, y, val)
  | x < a && y < b = (x, a - 1, val) : (a, y, v + val) : (y + 1, b, v) : fs
  | x < a && y == b = (x, a - 1, val) : (a, b, v + val) : fs
  | x < a && y > b = (x, a - 1, val) : (a, b, v + val) : insertNewQuery fs (b + 1, y, val)
  | x > a && y == b = (a, x - 1, v) : (x, b, v + val) : fs
  | x > a && y < b = (a, x - 1, v) : (x, y, v + val) : (y + 1, b, v) : fs
  | x > a && y > b = (a, x - 1, v) : (x, b, v + val) : insertNewQuery fs (b + 1, y, val)
  where
    (x, y, val) = newQuery
    (a, b, v) = f

performQueries = foldl insertNewQuery []

runTests = putStrLn tests
  where
    tests :: String
    tests =
      unlines
        [ testCase [(1, 2, 3), (4, 5, 6)] [(1, 2, 3), (4, 5, 6)],
          testCase [(4, 5, 6), (1, 2, 3)] [(1, 2, 3), (4, 5, 6)],
          testCase [(1, 2, 3), (2, 3, 4)] [(1, 1, 3), (2, 2, 7), (3, 3, 4)],
          testCase [(1, 2, 3), (1, 3, 4)] [(1, 2, 7), (3, 3, 4)],
          testCase [(1, 2, 3), (4, 5, 6), (7, 8, 9)] [(1, 2, 3), (4, 5, 6), (7, 8, 9)],
          testCase [] [],
          testCase [(10, 20, 3), (12, 15, 7)] [(10, 11, 3), (12, 15, 10), (16, 20, 3)],
          testCase [(12, 15, 7), (10, 20, 3)] [(10, 11, 3), (12, 15, 10), (16, 20, 3)],
          testCase [(10, 20, 3), (12, 22, 7)] [(10, 11, 3), (12, 20, 10), (21, 22, 7)],
          testCase [(12, 22, 7), (10, 20, 3)] [(10, 11, 3), (12, 20, 10), (21, 22, 7)]
        ]
    testCase queries expected = msg
      where
        got = performQueries queries
        success = got == expected
        testStatus = if success then "[SUCCESS]" else "[FAIL]"
        msg = testStatus ++ " performQueries " ++ show queries ++ " == " ++ show expected ++ fail_clause
        fail_clause = if success then "" else " but instead got " ++ show got

trd (_, _, z) = z

crush = maximum . fmap trd . performQueries

main = do
  [n, m] <- take 2 . readInts <$> getLine
  let readquery = (\[x, y, z] -> (x, y, z)) . take 3 . readInts <$> getLine
  queries <- replicateM m readquery
  print $ crush queries

readInt = read :: String -> Int

readInts = fmap readInt . words
