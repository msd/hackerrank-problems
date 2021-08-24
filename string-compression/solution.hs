countTopConsecutive :: Eq a => [a] -> Int
countTopConsecutive [] = 0
countTopConsecutive l = length $ takeWhile (==head l) l

compress :: String -> String
compress [] = []
compress s = [head s] ++ (if n > 1 then show n else "") ++ compress rest
    where
        n = countTopConsecutive s
        rest = drop n s


main :: IO ()
main = putStrLn . compress =<< getLine