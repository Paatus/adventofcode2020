p = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"

validFields =
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

takePart = takeWhile (/= ' ')

isValid fields = all (\f -> f fields) validationFuns
  where
    validationFuns = map elem validFields

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let passports = extractPassports filecontents []
      part1 = length $ filter (== True) $ map (isValid . parsePasswordFields) passports
  putStrLn ("Part 1: " ++ show part1)
