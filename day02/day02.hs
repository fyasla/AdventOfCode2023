import Data.Bool (Bool, not)
import Data.Char (digitToInt, isDigit)
import Data.Int (Int)
import Data.List (isPrefixOf)
import Data.List.Split
import Data.Maybe (fromJust, isJust)
import System.Console.Terminfo (Color (Blue))

main = interact solution

solution :: String -> String
solution = show . sum . map process . lines

getGameId :: String -> Int
getGameId = read . last . words . head . splitOn ":"

checkCubesPossible :: String -> Int -> Bool
checkCubesPossible c n = case c `compare` "green" of
  LT -> n <= 14 -- blue
  EQ -> n <= 13 -- green
  GT -> n <= 12 -- red

getGrabs :: String -> [String]
getGrabs = splitWhen (`elem` ",;") . last . splitOn ":"

checkGrabs :: [String] -> [Bool]
checkGrabs = map ((checkCubesPossible <$> last <*> (read . head)) . words)

checkGame :: String -> Bool
-- Return game Id if check possible 0 otherwise
checkGame = foldl (&&) True . checkGrabs . getGrabs

process :: String -> Int
process line = if checkGame line then getGameId line else 0
