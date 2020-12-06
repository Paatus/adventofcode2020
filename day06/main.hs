import Data.List
import qualified Data.Map as Map

extractGroups :: [String] -> [[String]]
extractGroups s = extractGroups' s []

extractGroups' :: [String] -> [[String]] -> [[String]]
extractGroups' [] acc = acc
extractGroups' str acc = extractGroups' newList (acc ++ [group])
  where
    newList = drop 1 $ dropWhile (/= "") str
    group = takeWhile (/= "") str

allSameInGroup :: [String] -> Int
allSameInGroup strs = Map.size $ Map.filter (== length strs) $ mapStringsInGroup strs

mapStringsInGroup :: [String] -> Map.Map Char Int
mapStringsInGroup = foldr mapString Map.empty

mapString :: String -> Map.Map Char Int -> Map.Map Char Int
mapString [] m = m
mapString (x : xs) m = mapString xs newM
  where
    newM = insertChar x m

insertChar :: Char -> Map.Map Char Int -> Map.Map Char Int
insertChar c = Map.insertWith (+) c 1

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let groups = extractGroups $ lines filecontents
      part1 = sum $ map (length . nub . concat) groups
      part2 = sum $ map allSameInGroup groups
  putStrLn ("Part 1: " ++ show part1 ++ "\n" ++ "Part 2: " ++ show part2)
