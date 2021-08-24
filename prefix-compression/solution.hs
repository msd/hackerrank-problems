countPrefix :: String -> String -> Int
countPrefix a b = length $ takeWhile id $ zipWith (==) a b

main :: IO ()
main = do
    x <- getLine
    y <- getLine
    let n = countPrefix x y
    let p = take n x
    let x' = drop n x
    let y' = drop n y
    let printWithLength s = putStrLn $ (show $ length s) ++ " " ++ s
    printWithLength p
    printWithLength x'
    printWithLength y'
