digitSum 0 = 0
digitSum x = let (d, m) = divMod x 10 in m + digitSum d

digitalRoot x = head $ dropWhile (>= 10) $ iterate digitSum x

work n k = digitalRoot $ k * digitalRoot n

main = do
  n : k : _ <- readIntList <$> getLine
  print $ work n k

--   print $ digitalRoot 9875

readInteger = read :: String -> Integer

readIntList = fmap readInteger . words
