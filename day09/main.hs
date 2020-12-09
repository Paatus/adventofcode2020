import Data.List

combinationsWRep :: [Int] -> Int -> [[Int]]
combinationsWRep xs n = filter ((n ==) . length . nub) $ mapM (const xs) [1 .. n]

isSumInPreamble :: Int -> [Int] -> Int -> Bool
isSumInPreamble preambleSize nums idx = (idx < preambleSize) || elem n prevSums
  where
    n = nums !! idx
    prevItems = (take preambleSize . drop (idx - preambleSize)) nums
    prevSums = nub $ map sum $ combinationsWRep prevItems 2

sumContSet' :: [Int] -> [Int] -> Int -> Int -> [Int] -> [Int]
sumContSet' _ [] _ _ acc = acc
sumContSet' origList (x : xs) target pass acc = case (sum acc == target, sum acc > target) of
  (True, _) -> acc
  (_, True) -> sumContSet' origList (drop pass origList) target (pass + 1) []
  _ -> sumContSet' origList xs target pass (x : acc)

solvePart2 target nums = (\x -> minimum x + maximum x) $ sumContSet' nums nums target 0 []

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let rows :: [Int]
      rows = map read $ lines filecontents
      withIndex = zip [0 ..] rows
      part1 = snd $ head $ dropWhile (\(idx, _) -> isSumInPreamble 25 rows idx) withIndex
      part2 = solvePart2 part1 rows
  putStrLn ("Part 1: " ++ show part1 ++ "\n" ++ "Part 2: " ++ show part2)
