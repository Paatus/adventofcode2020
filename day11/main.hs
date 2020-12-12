import qualified Data.Map.Strict as Map
import Data.Maybe

seatMap =
  [ "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL"
  ]

testVisibleOcc1 =
  [ ".......#.",
    "...#.....",
    ".#.......",
    ".........",
    "..#L....#",
    "....#....",
    ".........",
    "#........",
    "...#....."
  ]

testVisibleOcc2 =
  [ ".............",
    ".L.L.#.#.#.#.",
    "............."
  ]

theMap = buildMap seatMap

theMap2 = buildMap testVisibleOcc1

theMap3 = buildMap testVisibleOcc2

-- printMap :: Map.Map Pos Tile
-- printMap m =

splitEvery :: Int -> String -> [String] -> [String]
splitEvery n [] acc = acc
splitEvery n s acc = splitEvery n (drop n s) (acc ++ [take n s])

printMap :: Map.Map Pos Tile -> IO ()
printMap m = putStrLn $ unlines $ (\x -> splitEvery 10 x []) $ map (tileToChar . snd) $ Map.toList m

tileToChar :: Tile -> Char
tileToChar t = case t of
  Floor -> '.'
  Seat Empty -> 'L'
  Seat Occupied -> '#'

indexedMap :: [String] -> [(Int, [(Int, Tile)])]
indexedMap = zipWith (\y row -> (y, mapRow row)) [0 ..]
  where
    mapRow r = zipWith (\x c -> (x, getTile c)) [0 ..] r

data Seat = Empty | Occupied deriving (Show, Eq)

data Tile = Seat Seat | Floor deriving (Show, Eq)

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq)

type Pos = (Int, Int)

type IndexedTile = (Pos, Tile)

getTile :: Char -> Tile
getTile c = case c of
  '.' -> Floor
  'L' -> Seat Empty
  '#' -> Seat Occupied

indexRowToPos :: [IndexedTile] -> (Int, [(Int, Tile)]) -> [IndexedTile]
indexRowToPos acc (_, []) = acc
indexRowToPos acc (row, x : xs) = indexRowToPos (idxTile : acc) (row, xs)
  where
    idxTile = ((row, col), tile)
    col = fst x
    tile = snd x

buildMap :: [String] -> Map.Map Pos Tile
buildMap seatMap = Map.fromList indexedList
  where
    indexedList = indexRows >>= indexRowToPos []
    indexRows = indexedMap seatMap

set :: Tile -> Maybe Tile -> Maybe Tile
set newTile mT = case mT of
  Just Floor -> Just Floor
  Just (Seat _) -> Just newTile
  Nothing -> Nothing

takeSeats :: Pos -> Pos -> Map.Map Pos Tile -> Maybe Seat
takeSeats (y, x) (yoff, xoff) m = case Map.lookup currPos m of
  Nothing -> Nothing
  Just (Seat x) -> Just x -- takeSeats currPos (yoff, xoff) m (x : acc)
  _ -> takeSeats currPos (yoff, xoff) m
  where
    currPos = (y + yoff, x + xoff)

seatInDirection :: Pos -> Direction -> Map.Map Pos Tile -> Maybe Seat
seatInDirection pos dir m = case dir of
  N -> takeSeats pos (-1, 0) m
  NE -> takeSeats pos (-1, 1) m
  E -> takeSeats pos (0, 1) m
  SE -> takeSeats pos (1, 1) m
  S -> takeSeats pos (1, 0) m
  SW -> takeSeats pos (1, -1) m
  W -> takeSeats pos (0, -1) m
  NW -> takeSeats pos (-1, -1) m

seatsVisible :: Pos -> Map.Map Pos Tile -> [Seat]
seatsVisible pos m = mapMaybe (\d -> seatInDirection pos d m) [N, NE, E, SE, S, SW, W, NW]

getAdjacents :: Pos -> Map.Map Pos Tile -> [Seat]
getAdjacents pos m = seats
  where
    seats = mapMaybe extractSeat tiles
    tiles = getAdjacentTiles pos m
    extractSeat :: Maybe Tile -> Maybe Seat
    extractSeat t = case t of
      Just (Seat x) -> Just x
      _ -> Nothing

getAdjacentTiles :: Pos -> Map.Map Pos Tile -> [Maybe Tile]
getAdjacentTiles (y, x) m =
  map
    (`Map.lookup` m)
    [ (y - 1, x - 1),
      (y -1, x),
      (y -1, x + 1),
      (y, x - 1),
      (y, x + 1),
      (y + 1, x - 1),
      (y + 1, x),
      (y + 1, x + 1)
    ]

becomesOccupied :: Maybe Tile -> [Seat] -> Bool
becomesOccupied t seats = case (t, occAdjSeats) of
  (Just (Seat Empty), []) -> True
  _ -> False
  where
    occAdjSeats = filter (Occupied ==) seats

becomesEmpty :: Int -> Maybe Tile -> [Seat] -> Bool
becomesEmpty threshhold t seats = case (t, length occAdjSeats >= threshhold) of
  (Just (Seat Occupied), True) -> True
  _ -> False
  where
    occAdjSeats = filter (Occupied ==) seats

newState :: Map.Map Pos Tile -> Pos -> Tile -> Map.Map Pos Tile -> Map.Map Pos Tile
newState m pos _ accMap = case (occupied, empty) of
  (True, _) -> Map.alter (set (Seat Occupied)) pos accMap
  (_, True) -> Map.alter (set (Seat Empty)) pos accMap
  _ -> accMap
  where
    seat = Map.lookup pos m
    adjacents = getAdjacents pos m
    occupied = becomesOccupied seat adjacents
    empty = becomesEmpty 4 seat adjacents

newState2 :: Map.Map Pos Tile -> Pos -> Tile -> Map.Map Pos Tile -> Map.Map Pos Tile
newState2 m pos _ accMap = case (occupied, empty) of
  (True, _) -> Map.alter (set (Seat Occupied)) pos accMap
  (_, True) -> Map.alter (set (Seat Empty)) pos accMap
  _ -> accMap
  where
    seat = Map.lookup pos m
    adjacents = seatsVisible pos m
    occupied = becomesOccupied seat adjacents
    empty = becomesEmpty 5 seat adjacents

stepWith :: (Map.Map Pos Tile -> Pos -> Tile -> Map.Map Pos Tile -> Map.Map Pos Tile) -> (Map.Map Pos Tile -> Map.Map Pos Tile)
stepWith f m = Map.foldrWithKey (f m) m m

stepUntilStable :: Map.Map Pos Tile -> Map.Map Pos Tile
stepUntilStable m = if m == newM then newM else stepUntilStable newM
  where
    newM = stepWith newState m

stepUntilStable2 :: Map.Map Pos Tile -> Map.Map Pos Tile
stepUntilStable2 m = if m == newM then newM else stepUntilStable2 newM
  where
    newM = stepWith newState2 m

solveP1 :: Map.Map Pos Tile -> Int
solveP1 seatMap = Map.size $ Map.filter (Seat Occupied ==) $ stepUntilStable seatMap

solveP2 :: Map.Map Pos Tile -> Int
solveP2 seatMap = Map.size $ Map.filter (Seat Occupied ==) $ stepUntilStable2 seatMap

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let rows = lines filecontents
      m = buildMap rows
  putStrLn ("Part 1: " ++ show (solveP1 m) ++ "\n" ++ "Part 2: " ++ show (solveP2 m))
