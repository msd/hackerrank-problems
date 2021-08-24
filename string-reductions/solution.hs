import qualified Data.Set as Set

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates l = go (Set.fromList []) l
    where
        go _ [] = []
        go s (x:xs) = if x `Set.member` s
            then go s xs
            else x:(go (Set.insert x s) xs)

main :: IO ()
main = putStrLn . removeDuplicates =<< getLine