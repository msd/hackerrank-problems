--- Solved: 19/09/2022
--- Make tree-like art
--- Input integer n: 1 <= n <= 5

paddingChar = '_'

pictureWidth = 100

innerPictureWidth = pictureHeight

pictureHeight = 63

drawChar = '1'

centerPad :: Int -> String -> String
centerPad width x = leftPadding ++ x ++ rightPadding
  where
    remaining = max 0 (width - length x)
    (half, remainder) = remaining `divMod` 2
    leftWidth = half
    rightWidth = half + remainder
    leftPadding = replicate leftWidth paddingChar
    rightPadding = replicate rightWidth paddingChar

ramp = reverse . go
  where
    go n = take n $ iterate (paddingChar :) baseCase
    baseCase = [paddingChar, drawChar]

--- Make first element the new middle and copy the right elements (reversed) to the left side
mirror s = left_mid ++ right
  where
    right = drop 1 s
    left_mid = reverse s

centerPadLines w = fmap (centerPad w)

mirrorLines = fmap mirror

triangle = mirrorLines . ramp

line x = replicate x [drawChar]

level n = ramp n ++ line n

centeredLevel size width = centerPadLines width $ level size

-- paint = unlines . (centerPad pictureWidth <$>) . anchorBottom . level
paint n = unlines $ anchorBottom $ centerPadLines pictureWidth mirrored
  where
    smallest_power = 4 - n
    largest_power = 4 -- no reason, just spec
    powers = [smallest_power .. largest_power]
    sizes = (2 ^) <$> powers -- start small and double each time, last should be 16 aka 2^4
    unmirrored = reverse $ level <$> sizes
    mirrorAndPad :: Int -> [[String]] -> [String]
    mirrorAndPad _ [] = error "did not expect this"
    mirrorAndPad width [smallest_branch] = centerPadLines width $ mirrorLines smallest_branch
    mirrorAndPad width (big_branch : smaller_branches) = result
      where
        (half, rem) = width `divMod` 2
        leftWidth = half + rem
        rightWidth = half
        leftSide = mirrorAndPad leftWidth smaller_branches
        rightSide = mirrorAndPad rightWidth smaller_branches
        children = zipWith (++) leftSide rightSide
        result = children ++ centerPadLines width (mirrorLines big_branch)
    mirrored :: [String]
    mirrored = mirrorAndPad innerPictureWidth unmirrored

fillerLine = replicate pictureWidth paddingChar

anchorBottom list = replicate remaining fillerLine ++ list
  where
    remaining = max 0 $ pictureHeight - length list

readInt :: String -> Int
readInt = read

main = do
  n <- readInt <$> getLine
  putStrLn $ paint (n - 1)
