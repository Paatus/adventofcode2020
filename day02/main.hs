-- input =
--   [ "1-3 a: abcde",
--     "1-3 b: cdefg",
--     "2-9 c: ccccccccc"
--   ]

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

countLetter :: String -> Char -> Int
countLetter str c = length $ filter (== c) str

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let input = lines filecontents
  print
    ( length $
        filter (== True) $
          map (uncurry isValid . (\x -> (parseRule x, parsePassword x))) input
    )
