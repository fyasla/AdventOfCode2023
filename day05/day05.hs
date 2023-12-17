import Data.List.Split

main = interact solution'

getSeeds :: String -> [Int]
getSeeds = map (read :: String -> Int) . words . last . (splitOn ":") . head . lines

getSeeds' :: [Int] -> [(Int, Int)]
getSeeds' [] = []
getSeeds' (s : r : xs) = (s, s + r) : getSeeds' xs

isInSeedsRange :: [(Int, Int)] -> Int -> Bool
isInSeedsRange [] _ = False
isInSeedsRange (r : rs) x = (x >= fst r && x < snd r) || isInSeedsRange rs x

parseInput :: String -> [[(Int, Int, Int -> Int)]]
parseInput = map (map parseLine) . map (map (map (read :: String -> Int))) . map (map words) . map tail . tail . splitOn [""] . lines

sourceToDest :: [(Int, Int, Int -> Int)] -> Int -> Int
sourceToDest [] x = x
sourceToDest ((low, high, f) : ls) x
  | (x >= low) && (x < high) = f x
  | otherwise = sourceToDest ls x

parseLine :: [Int] -> (Int, Int, Int -> Int)
parseLine [dest, source, n] = (source, source + n, ((dest - source) +))

revParseLine :: [Int] -> (Int, Int, Int -> Int)
revParseLine [dest, source, n] = (dest, dest + n, ((source - dest) +))

revParseInput :: String -> [[(Int, Int, Int -> Int)]]
revParseInput = map (map revParseLine) . map (map (map (read :: String -> Int))) . map (map words) . map tail . tail . splitOn [""] . lines

getLocation :: String -> Int -> Int
getLocation input = foldl (flip (.)) id $ map sourceToDest (parseInput input)

getSeed :: String -> Int -> Int
getSeed input = foldl (.) id $ map sourceToDest (revParseInput input)

solution :: String -> String
solution input = show $ minimum $ map (getLocation input) (getSeeds input)

solution' :: String -> String
solution' input = show $ length $ takeWhile (not . (isInSeedsRange (getSeeds' $ getSeeds input))) $ map (getSeed input) [0 ..]
