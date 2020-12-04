import Data.Char

p = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"

validFieldNames =
  [ "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  ]

-- "cid"

f = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in\n"

extractPassports :: String -> [String] -> [String]
extractPassports [] acc = acc
extractPassports input acc =
  extractPassports remaining (acc ++ [pp])
  where
    pp = unwords (takeWhile (/= "") splitByLines)
    remaining = unlines $ drop 1 (dropWhile (/= "") splitByLines)
    splitByLines = lines input

replace _ _ [] = []
replace existing new (x : xs) =
  if x == existing
    then new : replace existing new xs
    else x : replace existing new xs

getFields = map (takeWhile (/= ':'))

getEntries :: [String] -> [(String, String)]
getEntries = map (\x -> (getField x, getVal x))
  where
    getField = takeWhile (/= ':')
    getVal = drop 1 . dropWhile (/= ':')

getPasswordParts :: String -> [String] -> [String]
getPasswordParts [] acc = acc
getPasswordParts s acc = getPasswordParts rest (acc ++ [part])
  where
    part = takePart pass
    rest = drop 1 $ dropWhile (/= ' ') pass
    pass = replace '\n' ' ' s

parsePasswordFields :: String -> [String]
parsePasswordFields s = getFields parts
  where
    parts = getPasswordParts s []

parsePassportEntries :: String -> [(String, String)]
parsePassportEntries s = getEntries parts
  where
    parts = getPasswordParts s []

takePart = takeWhile (/= ' ')

isValid fields = all (\f -> f fields) validationFuns
  where
    validationFuns = map elem validFieldNames

between :: Int -> Int -> Int -> Bool
between min max num = num >= min && num <= max

validateHairColor :: String -> Bool
validateHairColor hcl =
  length hcl == 7 && startsWithPound && allCharsInRange
  where
    startsWithPound = (==) '#' $ head hcl
    allCharsInRange = all (\x -> ord x `elem` validOrds) hcl
    validOrds = map ord "#0123456789abcdef"

validateHeight hgt =
  case unit of
    "cm" -> between 150 193 val
    "in" -> between 59 76 val
    _ -> False
  where
    unit = dropWhile isDigit hgt
    val = read $ takeWhile isDigit hgt

validatePassport :: [(String, String)] -> Bool
validatePassport passportEntries = all ((== True) . validateEntry) passportEntries && isValid fields
  where
    fields = map fst passportEntries

validateEntry :: (String, String) -> Bool
validateEntry (fieldName, value) =
  case (fieldName, value) of
    ("byr", v) -> between 1920 2002 (read v) -- (Birth Year) - four digits; at least 1920 and at most 2002.
    ("iyr", v) -> between 2010 2020 (read v) -- (Issue Year) - four digits; at least 2010 and at most 2020.
    ("eyr", v) -> between 2020 2030 (read v) -- (Expiration Year) - four digits; at least 2020 and at most 2030.
    ("hgt", v) -> validateHeight v -- (Height) - a number followed by either cm or in:
    -- If cm, the number must be at least 150 and at most 193.
    -- If in, the number must be at least 59 and at most 76.
    ("hcl", v) -> validateHairColor v -- (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ("ecl", v) -> any ((\f -> f v) . (==)) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] -- (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    ("pid", v) -> length v == 9 && all isDigit v -- (Passport ID) - a nine-digit number, including leading zeroes.
    ("cid", _) -> True
    _ -> False

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let passports = extractPassports filecontents []
      part1 = length $ filter (== True) $ map (isValid . parsePasswordFields) passports
      part2 = length $ filter (== True) $ map (validatePassport . parsePassportEntries) passports
  putStrLn ("Part 1: " ++ show part1 ++ "\n" ++ "Part 2: " ++ show part2)
