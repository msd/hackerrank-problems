{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad

factorial :: Int -> Int
factorial 0 = 1
factorial x = factorial (x - 1) * x

theFactorials :: [Int]
theFactorials = map factorial [0..9]

powers :: Int -> Double -> [Double]
powers n x = map ((x**) . fromIntegral) [0..n-1]

myexp :: Double -> Double
myexp x = sum $ zipWith (/) (powers 10 x) (map fromIntegral theFactorials)

main :: IO ()
main = do
    n <- readLn :: IO Int

    forM_ [1..n] $ \n_itr -> do
        x <- readLn :: IO Double
        print $ myexp x