import Data.List
import Data.List.Split
import System.Posix (BaudRate (B0))

-- getScore :: [String] -> [String] -> Int
-- getScore winning nums = let matches = length $ intersect winning nums
--   in if matches == 0 then 0 else 2^(matches - 1)

countMatchingNums :: [String] -> [String] -> Int
countMatchingNums winning nums = length $ intersect winning nums

getCardNumbers :: String -> [[String]]
getCardNumbers l = map words $ drop 1 $ splitWhen (`elem` ":|") $ head $ lines l

processLine :: String -> Int
processLine l = countMatchingNums (head a) (last a) where a = getCardNumbers l

-- solution :: String -> String
-- solution input = (show $ map processLine (lines input))

solution' :: String -> String
solution' input =
  let ls = map processLine (lines input)
   in show $ countCards ls (replicate (length ls) 1)

-- takes list of mathces and list of cards (initialized to : replicate (length matches) 1)
countCards :: [Int] -> [Int] -> Int
countCards matches [] = 0
countCards (m : ms) (c : cs) = c + countCards ms (zipWith (+) cs (replicate m c ++ [0, 0 ..]))

main = interact solution'
