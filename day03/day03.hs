import Data.Char (isDigit)
import Data.List
import GHC.Driver.Session (positionIndependent)

main = interact solution'

dimensions :: String -> (Int, Int)
dimensions = (,) <$> length . lines <*> length . head . lines

-- get all symbols (not digit neither '.') positions in list
-- symbolsPos :: [Char] -> [Int]
-- symbolsPos = findIndices (not . (`elem` '.':['0'..'9']))

-- second star
symbolsPos :: [Char] -> [Int]
symbolsPos = ('*' `elemIndices`)

allSymbolsPos :: [[Char]] -> [(Int, Int)]
allSymbolsPos schem = foldl1 (++) $ map parseSymbols $ zip [0 ..] $ map symbolsPos schem

parseSymbols :: (Int, [Int]) -> [(Int, Int)]
parseSymbols (i, xs) = [(i,)] <*> xs

-- get neighbours of position
squareNeighbours :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
squareNeighbours (m, n) (i, j) =
  [ (k, l) | k <- [i - 1, i, i + 1], l <- [j - 1, j, j + 1], (k, l) /= (i, j), k >= 0, k <= m, l >= 0, l <= n
  ]

-- get numbers present in a string
getNumbers :: [Char] -> [Int]
getNumbers [] = []
getNumbers (x : xs)
  | isDigit x = (read (takeWhile isDigit (x : xs)) :: Int) : getNumbers (dropWhile isDigit xs)
  | otherwise = getNumbers xs

goLeft :: String -> Int -> Int
goLeft l (-1) = (-1)
goLeft l i
  | not $ isDigit (l !! i) = i
  | otherwise = goLeft l (i - 1)

goRight :: String -> Int -> Int
goRight l i
  | i == length l = length l
  | not $ isDigit (l !! i) = i
  | otherwise = goRight l (i + 1)

-- given a schema dimension and a positon return a list of numbers that are adjacent to this position
-- Useful for stars
getNumbersAround :: (Int, Int) -> [[Char]] -> (Int, Int) -> [Int]
getNumbersAround (m, n) schem (i, j) = concatMap getNumbers [up, middle, down]
  where
    up = drop (goLeft (schem !! (i - 1)) (j - 1)) $ take (goRight (schem !! (i - 1)) (j + 1)) $ schem !! (i - 1)
    middle = drop (goLeft (schem !! i) (j - 1)) $ take (goRight (schem !! i) (j + 1)) $ schem !! i
    down = drop (goLeft (schem !! (i + 1)) (j - 1)) $ take (goRight (schem !! (i + 1)) (j + 1)) $ schem !! (i + 1)

-- map list of positions to positions around symbols and reduce ++
symbolsNeighbours :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
symbolsNeighbours (m, n) symbols = foldl (++) [] (map (squareNeighbours (m, n)) symbols)

-- filter keep only position where there is a digit
keepDigits :: [[Char]] -> [(Int, Int)] -> [(Int, Int)]
keepDigits schem positions = filter (\(i, j) -> isDigit (schem !! i !! j)) positions

-- get the starting position of each number
getStartingPos :: [[Char]] -> (Int, Int) -> (Int, Int)
getStartingPos _ (i, -1) = (i, 0)
getStartingPos schem (i, j)
  | isDigit $ schem !! i !! j = getStartingPos schem (i, j - 1)
  | otherwise = (i, j + 1)

-- filter starting numbers positions to be unique
-- nub

-- get number from starting position
getNum :: [[Char]] -> (Int, Int) -> Int
getNum schem (i, j) = read $ takeWhile isDigit $ snd $ splitAt j (schem !! i)

solution :: String -> String
solution input = show $ sum $ map (getNum $ lines input) $ nub $ map (getStartingPos $ lines input) $ keepDigits (lines input) $ symbolsNeighbours (n, m) $ allSymbolsPos $ lines input
  where
    (n, m) = dimensions input

solution' :: String -> String
solution' input = show $ foldl (\sum xs -> sum + (head xs) * (last xs)) 0 $ filter (\xs -> length xs == 2) $ map (getNumbersAround (dimensions input) (lines input)) $ allSymbolsPos (lines input)
