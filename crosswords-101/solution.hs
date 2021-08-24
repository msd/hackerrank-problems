import Data.Map ((?!))

splitHints :: String -> [String]
splitHints "" = []
splitHints s = h : (splitHints $ Prelude.drop (length h) s)
    where h = takeWhile (/= ';') s

newtype Vertex = Vertex Int
newtype Edge = Edge (Int, Int)
newtype Board = Board ([Vertex], [Edge])

countWordChars :: String -> Map Char [Int]
countWordChars w = go w 0
    where
        go (c:cs) pos = insertWith (++) c [pos] (go cs (pos + 1))
        go [] _ = empty

wordLetters :: [String] -> Map Char [(Int, Int)]
wordLetters words = Prelude.foldl (unionWith (++)) empty charsPerWord
    where
        charsPerWord = [((,) i <$>) <$> countWordChars w | (i, w) <- zip [0..] words]

main :: IO ()
main = do
    board <- readBoard
    hints <- splitHints <$> getLine
    let letts = wordLetters hints
    <$>
    putStrLn "todo"

lineCharacterCount :: Int
lineCharacterCount = 10

boardLineCount :: Int
boardLineCount = 10

readBoardLine :: IO String
readBoardLine = Prelude.take lineCharacterCount <$> getLine

readBoard :: IO [String]
readBoard = sequence $ replicate boardLineCount readBoardLine
