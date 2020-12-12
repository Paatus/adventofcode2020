import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.Maybe

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

initialMap :: IntMap.IntMap Int
initialMap = IntMap.fromList [(0, 1)]

-- Shamelessly Stolen from https://www.reddit.com/r/haskell/comments/kak5dz/advent_of_code_day_10_spoilers/gfbal3t?utm_source=share&utm_medium=web2x&context=3, but adapted to use my helper function.
populateMap :: [Int] -> IntMap.IntMap Int -> IntMap.IntMap Int
populateMap (x : xs) mmap = populateMap xs populatedMap
  where
    successors = getAvailNext (x : xs) x
    numPathsToX = fromMaybe 0 (IntMap.lookup x mmap)
    populatedMap = foldl (\newMap key -> IntMap.insertWith (+) key numPathsToX newMap) mmap successors
populateMap _ map = map

solveP2 :: [Int] -> Maybe Int
solveP2 joltages = IntMap.lookup (maximum joltageSteps) map
  where
    map = populateMap joltageSteps initialMap
    joltageSteps = sort (0 : joltages ++ [maxJoltage joltages])

maxJoltage :: [Int] -> Int
maxJoltage = (+) 3 . maximum

getDiffs :: [Int] -> (Int, Int) -> (Int, Int)
getDiffs [_] acc = acc
getDiffs (a : b : cs) (ones, threes) = case b - a of
  1 -> getDiffs (b : cs) (ones + 1, threes)
  3 -> getDiffs (b : cs) (ones, threes + 1)

solveP1 :: [Int] -> Int
solveP1 joltages = uncurry (*) $ getDiffs joltageSteps (0, 0)
  where
    joltageSteps = sort (0 : joltages ++ [maxJoltage joltages])

getAvailNext :: [Int] -> Int -> [Int]
getAvailNext ns n = filter (\x -> x - n < 4 && x - n > 0) ns

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let joltages = map read $ lines filecontents
  putStrLn
    ( "Part 1: "
        ++ show (solveP1 joltages)
        ++ "\n"
        ++ "Part 2: "
        ++ show (fromMaybe 0 (solveP2 joltages))
    )
