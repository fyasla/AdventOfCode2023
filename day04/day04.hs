import Data.List.Split
import Data.List

-- getScore :: [String] -> [String] -> Int
-- getScore winning nums = let matches = length $ intersect winning nums
--   in if matches == 0 then 0 else 2^(matches - 1)

countMatchingNums :: [String] -> [String] -> Int
countMatchingNums winning nums = length $ intersect winning nums

getCardNumbers :: String -> [[String]]
getCardNumbers l = map words $ drop 1 $ splitWhen (`elem` ":|") $ head $ lines l

processLine :: String -> Int 
processLine l = countMatchingNums (head a) (last a) where a = getCardNumbers l

solution :: String -> String
solution input = (show $ map processLine (lines input)) 

countCards :: [Int] -> Int
countCards matches = replicate (length matches) 1

main = interact solution
