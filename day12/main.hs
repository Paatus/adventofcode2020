data Direction = North | East | South | West deriving (Show, Eq, Enum)

data ActionType = N | S | W | E | L | R | F deriving (Show, Eq, Read)

type Action = (ActionType, Int)

newtype Latitude n = Lat n deriving (Show, Eq)

newtype Longitude n = Long n deriving (Show, Eq)

type Pos = (Latitude Int, Longitude Int)

type Ship = (Direction, Pos)

initialShip :: Ship
initialShip = (East, (Lat 0, Long 0))

rotate :: Direction -> Int -> Direction
rotate d deg = toEnum $ (`rem` 4) $ (+) 4 $ fromEnum d + steps
  where
    steps = deg `div` 90

rotateShip :: Action -> Ship -> Ship
rotateShip (aT, n) (dir, pos) = (newDir, pos)
  where
    newDir = case aT of
      L -> rotate dir (negate n)
      R -> rotate dir n
      _ -> dir

move :: Direction -> Int -> Pos -> Pos
move dir n (Lat lat, Long long) = case dir of
  North -> (Lat (lat + n), Long long)
  South -> (Lat (lat - n), Long long)
  East -> (Lat lat, Long (long + n))
  West -> (Lat lat, Long (long - n))

actionTypeToDirection :: ActionType -> Direction
actionTypeToDirection aT = case aT of
  N -> North
  S -> South
  E -> East
  W -> West

moveShip :: Action -> Ship -> Ship
moveShip (aT, n) (dir, pos) = (dir, newPos)
  where
    newPos = case aT of
      R -> pos
      L -> pos
      F -> move dir n pos
      d -> move (actionTypeToDirection d) n pos

applyInstruction :: Ship -> Action -> Ship
applyInstruction ship action = moveShip action $ rotateShip action ship

parseInstruction :: String -> Action
parseInstruction instr = (aT, n)
  where
    aT = read $ take 1 instr
    n = read $ drop 1 instr

applyInstructions :: [Action] -> Ship -> Ship
applyInstructions actions ship = foldl applyInstruction ship actions

wp :: Pos
wp = (Lat 1, Long 10)

moveToWaypoint :: Pos -> Int -> Ship -> Ship
moveToWaypoint (Lat lat, Long long) n ship = (fst ship, newPos)
  where
    (Lat sLat, Long sLong) = snd ship
    deltaLat = lat * n
    deltaLong = long * n
    newPos = (Lat (sLat + deltaLat), Long (sLong + deltaLong))

moveWaypoint :: Action -> Pos -> Pos
moveWaypoint (aT, n) (Lat lat, Long long) = case aT of
  N -> (Lat (lat + n), Long long)
  S -> (Lat (lat - n), Long long)
  E -> (Lat lat, Long (long + n))
  W -> (Lat lat, Long (long - n))
  _ -> (Lat lat, Long long)

rotateDirection :: ActionType -> Pos -> Pos
rotateDirection aT (Lat lat, Long long) = newPos
  where
    newPos = case aT of
      R -> (Lat (negate long), Long lat)
      L -> (Lat long, Long (negate lat))

rotateWaypoint :: Action -> Pos -> Pos
rotateWaypoint (aT, n) pos = newPos
  where
    newPos = case aT of
      L -> (!! (n `div` 90)) $ iterate (rotateDirection L) pos
      R -> (!! (n `div` 90)) $ iterate (rotateDirection R) pos
      _ -> pos

applyInstruction2 :: (Ship, Pos) -> Action -> (Ship, Pos)
applyInstruction2 (ship, pos) (aT, n) = case aT of
  F -> (moveToWaypoint pos n ship, pos)
  _ -> (ship, moveWaypoint (aT, n) $ rotateWaypoint (aT, n) pos)

applyInstructions2 :: [Action] -> (Ship, Pos) -> (Ship, Pos)
applyInstructions2 actions (ship, waypoint) = foldl applyInstruction2 (ship, waypoint) actions

solveP1 :: [Action] -> Ship -> Int
solveP1 actions ship = aLong + aLat
  where
    (_, (Lat lat, Long long)) = applyInstructions actions ship
    aLong = abs long
    aLat = abs lat

solveP2 :: [Action] -> Ship -> Int
solveP2 actions ship = aLong + aLat
  where
    ((_, (Lat lat, Long long)), _) = applyInstructions2 actions (ship, wp)
    aLong = abs long
    aLat = abs lat

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let instructions = map parseInstruction $ lines filecontents
      part1 = solveP1 instructions initialShip
      part2 = solveP2 instructions initialShip
  putStrLn ("Part 1: " ++ show part1 ++ "\n" ++ "Part 2: " ++ show part2)
