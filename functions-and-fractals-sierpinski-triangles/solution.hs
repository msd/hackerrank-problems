-- https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles/problem

 -- 32 rows and 63 columns 

triangle :: Char -> [String]
triangle c = [replicate (2 * i + 1) c | i <- [0..]]

ntrig :: Int -> Char -> [String]
ntrig h = (take h) . triangle

center :: Int -> Char -> String -> String
center w fill s = if length s >= w
    then -- no need for padding
        s
    else -- yes need padding
        let
            w' = (fromIntegral $ w - (length s)) / 2
            l = replicate (floor w') fill
            r = replicate (ceiling w') fill
        in
            l ++ s ++ r

empty :: Char
empty = '_'

full :: Char
full = '1'

rows :: Int
rows = 32

columns :: Int
columns = 63

sierpinski :: Int -> Int -> [String]
sierpinski 0 h = ntrig h full
sierpinski d h = sub ++ (foldl (zipWith (++)) sub [sub_empty, sub])
    where
        sub_height = h `div` 2
        sub = sierpinski (d - 1) sub_height
        sub_empty = reverse $ ntrig sub_height empty

main :: IO ()
main = do
    n <- readLn :: IO Int
    putStrLn . unlines . (center columns empty <$>) $ sierpinski n rows
