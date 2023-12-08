import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)

digitMap :: [(String, Int)]
digitMap = 
  [("one", 1)
  ,("two", 2)
  ,("three", 3)
  ,("four", 4)
  ,("five", 5)
  ,("six", 6)
  ,("seven", 7)
  ,("eight", 8)
  ,("nine", 9)]

main = interact calibrationSum

calibrationSum :: String -> String
calibrationSum = (++ "\n") . show . sum . map process2 . lines

process :: String -> Int
process = (\xs -> 10 * head xs + last xs) . map digitToInt . filter isDigit

firstDigit :: String -> [(String, Int)] -> Int
firstDigit s dico 
  | s == "" = 0
  | isDigit $ head s = digitToInt $ head s
  | isJust $ spelled = fromJust spelled
  | otherwise = firstDigit (tail s) dico
  where spelled = foldl (\acc (k, v) -> if (`isPrefixOf` s) $ k then Just v else acc) Nothing dico 

process2 :: String -> Int
process2 = (\xs -> 10 * (firstDigit xs digitMap) + firstDigit (reverse xs) (map (\(k, v) -> (reverse k, v)) digitMap))
