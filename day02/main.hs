type Rule = (Char, Int, Int)

parseRule :: String -> Rule
parseRule str = (c, min, max)
  where
    min = read $ takeWhile (/= '-') str
    max = read $ (takeWhile (/= ' ') . dropWhile (== '-') . dropWhile (/= '-')) str
    c = ((!! 1) . dropWhile (/= ' ')) str

parsePassword :: String -> String
parsePassword = drop 2 . dropWhile (/= ':')

isValid :: Rule -> String -> Bool
isValid (c, min, max) str =
  count >= min && count <= max
  where
    count = countLetter str c

isValid2 :: Rule -> String -> Bool
isValid2 (c, min, max) str =
  (== 1) $ length $ filter (== c) [str !! m, str !! x]
  where
    m = min - 1
    x = max - 1

countLetter :: String -> Char -> Int
countLetter str c = length $ filter (== c) str

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let input = lines filecontents
      part1 = show $ length (filter (== True) $ map (uncurry isValid . (\x -> (parseRule x, parsePassword x))) input)
      part2 = show $ length (filter (== True) $ map (uncurry isValid2 . (\x -> (parseRule x, parsePassword x))) input)
  putStrLn
    ( "Part 1: " ++ part1 ++ "\n"
        ++ "Part 2: "
        ++ part2
    )
