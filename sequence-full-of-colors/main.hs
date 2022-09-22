import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

data Color = B | G | R | Y deriving (Show, Eq, Read, Ord)

work :: [Color] -> Bool
work list = reds == greens && yellows == blues && gor
  where
    reds = Map.findWithDefault 0 R counts
    blues = Map.findWithDefault 0 B counts
    yellows = Map.findWithDefault 0 Y counts
    greens = Map.findWithDefault 0 G counts
    (counts, gor) = go list Map.empty
    go :: [Color] -> Map.Map Color Int -> (Map.Map Color Int, Bool)
    go [] m = (m, True)
    go (x : xs) m = if cond then (next_map, True) else (new_map, False) -- short circuit
      where
        count_x = 1 + Map.findWithDefault 0 x m
        new_map = Map.insert x count_x m
        reds_now = Map.findWithDefault 0 R new_map
        blues_now = Map.findWithDefault 0 B new_map
        yellows_now = Map.findWithDefault 0 Y new_map
        greens_now = Map.findWithDefault 0 G new_map
        (next_map, next_cond) = go xs new_map
        cond = abs (reds_now - greens_now) <= 1 && abs (yellows_now - blues_now) <= 1 && next_cond

toList :: a -> [a]
toList x = [x]

main = do
  t <- readInt <$> getLine
  cases <- replicateM t $ fmap (readColor . toList) <$> getLine
  mapM_ (print . work) cases

readInt = read :: String -> Int

readColor = read :: String -> Color
