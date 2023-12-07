import Data.Char (digitToInt, isDigit)

main = interact calibrationSum

myfunc :: String -> String
myfunc s = show $ length s

calibrationSum :: String -> String
calibrationSum = (++ "\n") . show . sum . map process . lines

process :: String -> Int
process = (\xs -> 10 * head xs + last xs) . map digitToInt . filter isDigit
