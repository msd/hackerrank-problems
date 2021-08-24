swapChars :: String -> String
swapChars (a:b:rest) = b:a:(swapChars rest)
swapChars _ = []

main :: IO ()
main = do
    t <- readLn :: IO Int
    lines_ <- sequence $ replicate t getLine
    putStr $ unlines (swapChars <$> lines_)
-- main = putStr =<< unlines <$> (sequence . (flip replicate (swapChars <$> getLine)) =<< (readLn :: IO Int))
