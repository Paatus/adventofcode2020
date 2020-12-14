import Data.List
import Debug.Trace

nums :: [Int]
nums = [7, 13, 59, 31, 19]

s = "7,13,x,x,59,x,31,19"

firstDepartureAfter :: Int -> Int -> Int
firstDepartureAfter timestamp busId = head $ dropWhile (< timestamp) $ iterate (busId +) 0

parseList :: String -> [String] -> [String]
parseList [] acc = acc
parseList s acc = parseList remaining (acc ++ [part])
  where
    remaining = drop 1 $ dropWhile (/= ',') s
    part = takeWhile (/= ',') s

parseInput :: [String] -> (Int, [Int])
parseInput (x : y : _) = (ts, busIds)
  where
    ts = read x
    busIds = map read $ filter (/= "x") $ parseList y []

solveP1 :: Int -> [Int] -> Int
solveP1 ts bIds = minutesDiff * busId
  where
    departures = map (\id -> (id, firstDepartureAfter ts id)) bIds
    (busId, departureTs) = minimumBy (\a b -> if snd a <= snd b then LT else GT) departures
    minutesDiff = departureTs - ts

isDeparture :: Int -> Int -> Bool
isDeparture busId n = n `rem` busId == 0 && n `elem` [0, busId .. n]

matchesPatternLazy :: [(Int, Int)] -> Int -> Bool
matchesPatternLazy [] n = True
matchesPatternLazy ((offset, busId) : xs) n = isDeparture busId (n + offset) && matchesPatternLazy xs n

matchesPattern :: [(Int, Int)] -> Int -> Bool
matchesPattern c n = trace (show validators) $ not $ any (/= True) validators
  where
    validators = map (\(offset, busId) -> isDeparture busId (n + offset)) c

solveP2 :: String -> Int
solveP2 s = head $ filter (matchesPatternLazy (drop 1 input)) firstBusDepartures
  where
    input :: [(Int, Int)]
    input = map (\(a, b) -> (a, read b)) $ filter ((/=) "x" . snd) $ zip [0 ..] $ parseList s []
    firstBusDepartures = (\(_, n) -> [0, n ..]) $ head input

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let (ts, busIds) = parseInput $ lines filecontents
      p2Input = lines filecontents !! 1
      part1 = solveP1 ts busIds
      part2 = solveP2 p2Input
  putStrLn ("Part 1: " ++ show part1 ++ "\nPart 2: " ++ show part2)
