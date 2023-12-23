import Data.List.Split
import Data.List
import qualified Data.Map as Map

type Node = String
type Instructions = String
type Plan = Map.Map Node (Node, Node)

parseLine :: String -> (Node, (Node, Node))
parseLine s = (key, (left, right)) where
  key = head $ splitOn " = " s
  tuple = splitOn ", " $ last $ splitOn " = " s
  left = tail $ head tuple
  right = init $ last tuple

getPlan :: String -> Plan
getPlan = Map.fromList . getListPlan

getListPlan :: String -> [(Node,(Node,Node))]
getListPlan = map parseLine . tail . tail . lines

getStartNodes :: String -> [Maybe Node]
getStartNodes = map (Just . fst) . filter (\(n, _) -> last n == 'A') . getListPlan

getInstructions :: String -> Instructions
getInstructions = head . lines

nextNode ::  Plan -> Char -> Node -> Maybe Node
nextNode p 'L' n = fmap fst $ Map.lookup n p
nextNode p 'R' n = fmap snd $ Map.lookup n p
nextNode _ _ _ = Nothing

navigate :: Int -> Plan -> Instructions -> Maybe Node -> Int
navigate count _ _ (Just "ZZZ") = count
navigate count plan (dir:instr) mNode = navigate (count + 1) plan instr mNewNode
  where mNewNode = mNode >>= nextNode plan dir

endAllWithZ :: [Maybe Node] -> Bool
endAllWithZ = all (\x -> (last <$> x) == Just 'Z')

navigate' :: Int -> Plan -> Instructions -> [Maybe Node] -> Int
navigate' count plan (dir:instr) mList
  | endAllWithZ mList = count
  | otherwise = navigate' (count + 1) plan instr mNewList
    where mNewList = nub $ map (\mNode -> mNode >>= nextNode plan dir) mList

solution :: String -> String
solution input = show $ navigate 0 (getPlan input) (cycle $ getInstructions input) $ Just "AAA"

solution' :: String -> String
solution' input = show $ navigate' 0 (getPlan input) (cycle $ getInstructions input) $ getStartNodes input 

main = interact solution'
