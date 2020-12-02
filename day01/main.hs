subsets :: Integer -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

is2020 :: (Num a, Eq a) => [a] -> Bool
is2020 xs = (==) 2020 $ sum xs

solve :: (Eq a, Num a) => [[a]] -> a
solve subsets = head $ map product $ filter is2020 subsets

part1 :: (Eq a, Show a, Num a) => [a] -> String
part1 nums = show $ solve $ subsets 2 nums

part2 :: (Eq a, Show a, Num a) => [a] -> String
part2 nums = show $ solve $ subsets 3 nums

main :: IO ()
main = do
  filecontents <- readFile "input"
  let nums = map read $ lines filecontents
  putStrLn
    ( "Part 1: " ++ part1 nums ++ "\n"
        ++ "Part 2: "
        ++ part2 nums
        ++ "\n"
    )
