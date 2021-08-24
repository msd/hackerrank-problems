{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set

outStr = "Hello World"

-- Print "Hello World" on a new line 'n' times.
main :: IO()
main = do
    n <- readLn :: IO Int
    myprint n
        where myprint 1 = putStrLn outStr
              myprint x = do
                putStrLn outStr
                myprint (x - 1)
