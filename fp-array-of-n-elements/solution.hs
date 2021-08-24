fn n = replicate n 0

main = do
    n <- readLn :: IO Int
    print (fn n)