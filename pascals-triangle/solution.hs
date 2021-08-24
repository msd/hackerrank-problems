consecutivePairs :: [a] -> [(a, a)]
consecutivePairs (x:rest@(y:z)) = (x, y) : (consecutivePairs rest)
consecutivePairs _ = []

padZeros :: Num a => [a] -> [a]
padZeros = ([0] ++) . (++ [0])

triangleRow :: [Int] -> [Int]
triangleRow = (map $ uncurry (+)) . consecutivePairs . padZeros

triangle :: [[Int]]
triangle = [1]: go [1]
    where
        go acc = let tr = triangleRow acc in tr : (go tr)

main :: IO ()
main = putStrLn =<< ( unlines . (map (unwords . (map show))) . (flip take triangle) <$> readLn)
