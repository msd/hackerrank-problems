mingle (a:as) (b:bs) = a: b: (mingle as bs)
mingle _ _ = []

main :: IO ()
main = putStrLn =<< (mingle <$> getLine <*> getLine)