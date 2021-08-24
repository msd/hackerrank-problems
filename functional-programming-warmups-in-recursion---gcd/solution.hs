my_gcd :: Int -> Int -> Int
my_gcd x 0 = x
my_gcd x y = if y > x then my_gcd y x else my_gcd y (x `mod` y)

main :: IO ()
main = do
    l <- getLine
    let (x:y: _) = (read :: String -> Int) <$> (words l)
    print $ my_gcd x y
