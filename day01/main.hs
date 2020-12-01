-- nums = [1721, 979, 366, 299, 675, 1456]

subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs


is2020 :: (Num a, Eq a) => [a] -> Bool
is2020 xs = (==) 2020 $ sum
    where sum = foldl (+) 0 xs


multiplyNums :: Num a => [a] -> a
multiplyNums = foldl (*) 1

main :: IO ()
main = do
    filecontents <- readFile "input"
    let nums = map read $ lines filecontents
    putStrLn $ show $ head $ map multiplyNums $ filter is2020 $ subsets 2 nums
