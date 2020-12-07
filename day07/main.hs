import Data.Char
import Data.List

type Bag = (String, Int)

type Rule = (String, [Bag])

parseRule :: String -> (String, [Bag])
parseRule s = (bagsName, bags)
  where
    bagsName = unwords $ take 2 $ takeWhile (/= "contain") $ words s
    bags = map parseBag bagStrings
    bagStrings = takeBags (unwords $ drop 1 $ dropWhile (/= "contain") $ words s) []

parseBag :: String -> Bag
parseBag bagStr = (name, quant)
  where
    quant = case length $ filter isDigit bagStr of
      0 -> 0
      _ -> read $ takeWhile isDigit bagStr
    name = unwords $ take 2 $ words $ dropWhile (\x -> isDigit x || x == ' ') bagStr

takeBags :: String -> [String] -> [String]
takeBags [_] acc = acc
takeBags s acc = takeBags rest (bag : acc)
  where
    rest = dropWhile (\x -> x == ',' || x == ' ') $ dropWhile firstBagCondition s
    bag = takeWhile firstBagCondition s
    firstBagCondition = \x -> x /= ',' && x /= '.'

canContain :: String -> Rule -> Bool
canContain bagName (_, canContainBags) = (>= 1) $ length $ filter ((==) bagName . fst) canContainBags

allCanContain :: String -> [Rule] -> [String] -> [String]
allCanContain bagName rules acc = case bags of
  [] -> acc
  bagColors -> bagColors >>= (\x -> allCanContain x rules (x : acc))
  where
    bags = map fst $ filter (canContain bagName) rules

part1 :: [Rule] -> Int
part1 rules = n
  where
    n = length $ nub $ allCanContain "shiny gold" rules []

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let rules = map parseRule $ lines filecontents
  print (part1 rules)
