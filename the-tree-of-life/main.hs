import Data.Bits

import Prelude hiding (Left, Right, traverse)

data Direction = Left | Right deriving (Eq)

instance Show Direction where
    show Left = "<"
    show Right = ">"

type Path = [Direction]

data Tree a
    = Node a (Tree a) (Tree a)
    | Leaf a
    deriving (Eq, Show)

getValue p t = valueHere <$> traverse p t

traverse :: Path -> Tree a -> Maybe (Tree a)
traverse [] t = Just t
-- more path but no more traversing options
traverse _ (Leaf _) = Nothing
-- traverse
traverse (Left:rest) (Node _ l r) = traverse rest l 
traverse (Right:rest) (Node _ l r) = traverse rest r

valueHere (Leaf v) = v
valueHere (Node v _ _) = v

shifter list = bitOr bits
    where
        zipped = zip [0..] (reverse list)
        bits = bit . fst <$> filter snd zipped

bitAnd :: Bits a => [a] -> a
bitAnd = foldl (.&.) zeroBits

bitOr :: Bits a => [a] -> a
bitOr = foldl (.|.) zeroBits

func :: Int -> Bool -> Bool -> Bool -> Bool -> Bool
func funcNo parent leftChild current rightChild = bools !! index
    where
        index = shifter [parent, leftChild, current, rightChild]
        bools = testBit funcNo <$> [0..15]

myassert x y msg = putStrLn $ if x == y then "PASS" else ("FAILED expected " ++ show x ++ " but got " ++ show y ++ ", msg: " ++  msg)

work f =
        [ (f True  True  True  True,   "1111")
        , (f True  True  True  False,  "1110")
        , (f True  True  False True,   "1101")
        , (f True  True  False False,  "1100")
        , (f True  False True  True,   "1011")
        , (f True  False True  False,  "1010")
        , (f True  False False True,   "1001")
        , (f True  False False False,  "1000")
        , (f False True  True  True,   "0111")
        , (f False True  True  False,  "0110")
        , (f False True  False True,   "0101")
        , (f False True  False False,  "0100")
        , (f False False True  True,   "0011")
        , (f False False True  False,  "0010")
        , (f False False False True,   "0001")
        , (f False False False False,  "0000")
        ]

main = do
    -- [n, a,b,c,d] <- readIntList <$> getLine
    -- print $ func n (a==1) (b==1) (c==1) (d==1)
    -- Rule 7710: (An analogue of Rule 30)

    let expected = toNum <$> [
                False, False, False, True,
                True, True, True, False,
                False, False, False, True,
                True, True, True, False
            ]
    let got = toNum . fst <$> work (func 7710)
    putStrLn $ unlines $ show <$> zip got expected
    print $ toNum <$> (testBit (7710::Int) <$> [15,14..0])

deleteAll _ [] = []
deleteAll x (e:rest)
    | x == e = deleteAll x rest
    | otherwise = e: deleteAll x rest

readTree :: String -> Tree Bool
readTree (first:rest) = undefined
    where
        no_spaces = deleteAll ' ' s
        left = takeWhile (/=')') no_spaces
        -- after_left = drop (length left) no_spaces

toNum False = 0
toNum True = 1

readInt = read :: String -> Int

readIntList = fmap readInt . words
