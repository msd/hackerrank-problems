len :: [a] -> Int
len (x:xs) = 1 + (len xs)
len _ = 0

-- main = do
--     inputdata <- getContents
--     putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata