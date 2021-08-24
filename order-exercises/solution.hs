readNumbers :: (Num a, Read a) => IO [a]
readNumbers = getLine >>= (return . (map read) . words)

main :: IO ()
main = do
    nums0 <- readNumbers >>= (return . (take 2))
    let n = nums0 !! 0
    let k = nums0 !! 1
    nums1 <- readNumbers >>= (return . (take n))
    return $ whatever nums1
    return ()

-- ordered

whatever :: [Int] -> [Int]
whatever [x] = [x]
whatever (h:rest) = 
    max (head restwhatever + h) h : restwhatever
    where
        restwhatever = whatever rest