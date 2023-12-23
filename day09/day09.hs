

extrapolate :: [Int] -> Int
extrapolate xs
  | all (== 0) xs = 0
  | otherwise = (last xs) + (extrapolate $ zipWith (-) (tail xs) (init xs))

solution :: String -> String
solution = show . sum . (map extrapolate) . map (map (read :: String -> Int)) . map words . lines

solution' :: String -> String
solution' = show . sum . (map extrapolate) . map (map (read :: String -> Int)) . map reverse . map words . lines

main = interact solution'
