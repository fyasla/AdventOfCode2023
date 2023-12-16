import Data.List.Split

main = interact solution

getSeeds :: String -> [Int]
getSeeds = map (read :: String -> Int) . words . last . (splitOn ":") . head . lines

parseInput :: String -> [[(Int, Int, Int -> Int)]]
parseInput = map (map parseLine) . map (map (map (read :: String -> Int))) . map (map words) . map tail . tail . splitOn [""] . lines

sourceToDest :: [(Int, Int, Int -> Int)] -> Int -> Int
sourceToDest [] x = x
sourceToDest ((low, high, f) : ls) x
  | (x >= low) && (x < high) = f x
  | otherwise = sourceToDest ls x

parseLine :: [Int] -> (Int, Int, Int -> Int)
parseLine [dest, source, n] = (source, source + n, ((dest - source) +))

getLocation :: String -> Int -> Int
getLocation input = foldl (flip (.)) id $ map sourceToDest (parseInput input)

solution :: String -> String
solution input = show $ minimum $ map (getLocation input) (getSeeds input)
