import Data.List

type Range = (Int, Int)

parseBoardingPass :: String -> (Int, Int, Int)
parseBoardingPass pass = (row, col, seatId)
  where
    row = fst $ foldr ($) (0, 127) $ reverse $ map binaryTake (take 7 pass)
    col = fst $ foldr ($) (0, 7) $ reverse $ map binaryTake ((take 3 . drop 7) pass)
    seatId = row * 8 + col

binaryTake :: Char -> Range -> Range
binaryTake c (min, max) = case c of
  'F' -> (min, middle)
  'B' -> (middle + 1, max)
  'L' -> (min, middle)
  'R' -> (middle + 1, max)
  where
    middle = (min + max) `div` 2

trd :: (a, a, a) -> a
trd (_, _, a) = a

missingEl :: [Int] -> Int
missingEl (a : b : cs) = case b - a of
  1 -> missingEl (b : cs)
  _ -> a + 1

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let passwords = lines filecontents
      part1 = maximum $ map (trd . parseBoardingPass) passwords
      part2 = missingEl $ sort $ map (trd . parseBoardingPass) passwords
  putStrLn
    ( "Part 1: " ++ show part1 ++ "\n"
        ++ "Part 2: "
        ++ show part2
    )
