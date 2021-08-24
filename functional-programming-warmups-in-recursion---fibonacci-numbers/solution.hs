fib :: [Int]
fib = 0:1: (zipWith (+) fib (tail fib))

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ fib !! (n - 1)