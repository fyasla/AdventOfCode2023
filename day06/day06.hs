import Data.List.Split

possibleTimes :: Int -> [Int]
possibleTimes x = zipWith (*) [0 .. x] [x, x - 1 .. 0]

getTimes :: String -> [Int]
getTimes = map read . head . map (words . last . splitOn ":") . lines

getDistances :: String -> [Int]
getDistances = map read . last . map (words . last . splitOn ":") . lines

numberOfWays :: Int -> Int -> Int
numberOfWays record time = length $ filter (> record) $ possibleTimes time

solution :: String -> String
solution input = show $ product $ zipWith numberOfWays (getDistances input) (getTimes input)

main = interact solution
