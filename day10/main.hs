import Data.List

joltages :: [Int]
joltages =
  [ 16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4
  ]

joltages2 :: [Int]
joltages2 =
  [ 28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3
  ]

maxJoltage :: [Int] -> Int
maxJoltage = (+) 3 . maximum

getDiffs :: [Int] -> (Int, Int) -> (Int, Int)
getDiffs [_] acc = acc
getDiffs (a : b : cs) (ones, threes) = case b - a of
  1 -> getDiffs (b : cs) (ones + 1, threes)
  3 -> getDiffs (b : cs) (ones, threes + 1)

solveP1 joltages = uncurry (*) $ getDiffs joltageSteps (0, 0)
  where
    joltageSteps = sort (0 : joltages ++ [maxJoltage joltages])

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let joltages = map read $ lines filecontents
  print $ solveP1 joltages
