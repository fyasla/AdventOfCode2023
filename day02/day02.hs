import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)
import Data.List.Split

main = interact solution

solution :: String -> String
solution = (++ "\n") . show . sum . map process . lines

process :: String -> Int
process s = length s
