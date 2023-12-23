import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Function

solution :: String -> String
solution input = show . sum $ zipWith (*) (sortBids input) [1..] 

main = interact solution

partialScore :: String -> [Int]
partialScore = reverse . sort . map length . group . sort . unjokerize . head . words

completeScore :: String -> [Int]
completeScore = (++) <$> partialScore <*> map parseChar . head . words

parseChar :: Char -> Int
parseChar c 
  | isDigit(c) = digitToInt(c)
  | c == 'J' = 1
  | c == 'T' = 10
  | c == 'Q' = 12
  | c == 'K' = 13
  | c == 'A' = 14

unjokerize :: String -> String
unjokerize "JJJJJ" = "AAAAA"
unjokerize s = map (replaceJ (mainChar s)) s

countChar :: String -> [[(Int, Char)]]
countChar s =  group $ sort $ zip [1,1..] s

mainChar :: String -> Char
mainChar = snd . head . maximumBy (compare `on` length) . countChar . filter (/= 'J')

replaceJ :: Char -> Char -> Char 
replaceJ c x
  | 'J' == x = c
  | otherwise = x

sortBids :: String -> [Int]
sortBids input = map (read . snd) $ sort $ zip (map completeScore $ lines input) ( map (last . words) $ lines input)
