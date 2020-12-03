type Map = [String]

type Direction = (Int, Int)

type Pos = (Int, Int)

itemOf :: [a] -> Int -> Maybe a
xs `itemOf` x = let xslen = length xs in if abs x > xslen then Nothing else Just (xs !! (x `mod` xslen))

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

buildMapN :: Int -> [[String]] -> Map
buildMapN n part = map concat $ transpose $ take n $ extendMap part

extendMap :: [[String]] -> [[String]]
extendMap part = concat $ repeat part

charAt :: Pos -> Map -> Maybe Char
charAt (x, y) m = case m `itemOf` y of
  Nothing -> Nothing
  Just row -> row `itemOf` x

travel :: Pos -> Direction -> Map -> [Char] -> [Char]
travel (x, y) (r, d) map acc =
  case newChar of
    Nothing -> acc
    Just c -> travel (newX, newY) (r, d) map (c : acc)
  where
    newChar = charAt (newX, newY) map
    newX = x + r
    newY = y + d

getTreesForDirection :: Map -> Direction -> Int
getTreesForDirection m dir = length $ filter (== '#') $ travel (0, 0) dir m []

getTreesForDirections :: Map -> [Direction] -> [Int]
getTreesForDirections m = map (getTreesForDirection m)

getRequiredWidth :: [[String]] -> Direction -> Int
getRequiredWidth (x : _) (r, d) = reqWidth
  where
    width = length $ head x
    c = width `div` d
    reqWidth = c * r

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let input = lines filecontents
      requiredWidth1 = getRequiredWidth [input] (3, 1)
      requiredWidth2 = getRequiredWidth [input] (7, 1)
      part1 = getTreesForDirection (buildMapN requiredWidth1 [input]) (3, 1)
      part2 = product $ getTreesForDirections (buildMapN requiredWidth2 [input]) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  putStrLn ("Part 1: " ++ show part1 ++ "\n" ++ "Part 2: " ++ show part2)
