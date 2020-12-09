import Data.Maybe

data Instruction n = Acc n | Jmp n | Nop n deriving (Show, Eq)

parseInstr :: String -> Instruction Int
parseInstr s = case takeWhile (/= ' ') s of
  "nop" -> Nop n
  "acc" -> Acc n
  "jmp" -> Jmp n
  where
    n = case head rawVal of
      '+' -> read $ drop 1 rawVal
      '-' -> negate $ read $ drop 1 rawVal
    rawVal = dropWhile (\x -> x /= '+' && x /= '-') s

stepState :: Instruction Int -> (Int, Int) -> (Int, Int)
stepState instruction (pos, acc) = case instruction of
  Acc n -> (pos + 1, acc + n)
  Nop _ -> (pos + 1, acc)
  Jmp n -> (pos + n, acc)

finishes :: [Instruction Int] -> Maybe (Int, Int) -> [Int] -> Bool
finishes instructions lastState poss = case (exists, elemAt instructions pos) of
  (_, Nothing) -> True
  (True, _) -> False
  _ -> finishes instructions (Just s) (poss ++ [fst s])
  where
    s = stepState (instructions !! pos) (fromMaybe (0, 0) lastState)
    pos = maybe 0 fst lastState
    exists = fst s `elem` poss

stateBeforeHalt :: [Instruction Int] -> Maybe (Int, Int) -> [Int] -> Int
stateBeforeHalt instructions lastState poss =
  if exists
    then snd s
    else stateBeforeHalt instructions (Just s) (poss ++ [fst s])
  where
    s = stepState (instructions !! pos) (fromMaybe (0, 0) lastState)
    pos = maybe 0 fst lastState
    exists = fst s `elem` poss

runInstructions :: [Instruction Int] -> [Instruction Int] -> [(Int, Int)] -> [(Int, Int)]
runInstructions actions ranInstructions acc =
  if isNothing (elemAt actions (fst l))
    then acc
    else runInstructions actions (ranInstructions ++ [a]) newAcc
  where
    a = actions !! fst l
    newAcc = acc ++ [stepState a l]
    l = case acc of
      [] -> (0, 0)
      a -> last a

elemAt :: [a] -> Int -> Maybe a
elemAt as idx =
  if idx >= length as
    then Nothing
    else Just (as !! idx)

switchJmpNop :: Instruction Int -> Instruction Int
switchJmpNop n = case n of
  Nop n -> Jmp n
  Jmp n -> Nop n
  a -> a

isNop :: Instruction a -> Bool
isNop i = case i of
  Nop _ -> True
  _ -> False

isJmp :: Instruction a -> Bool
isJmp i = case i of
  Jmp _ -> True
  _ -> False

replaceNthJmp :: [Instruction Int] -> Int -> Maybe [Instruction Int]
replaceNthJmp instructions n = case jmpIdx of
  Nothing -> Nothing
  Just n -> Just (take n instructions ++ [switchJmpNop (instructions !! n)] ++ drop (n + 1) instructions)
  where
    itemsWithIdx = zip [0 ..] instructions
    nops = filter (\(_, instr) -> isJmp instr) itemsWithIdx
    jmpIdx = fst <$> elemAt nops n

replaceNthNop :: [Instruction Int] -> Int -> Maybe [Instruction Int]
replaceNthNop instructions n = case nopIdx of
  Nothing -> Nothing
  Just n -> Just (take n instructions ++ [switchJmpNop (instructions !! n)] ++ drop (n + 1) instructions)
  where
    itemsWithIdx = zip [0 ..] instructions
    nops = filter (\(_, instr) -> isNop instr) itemsWithIdx
    nopIdx = fst <$> elemAt nops n

tryReplacingJmps instructions idx = case newInstructions of
  Nothing -> Nothing
  Just instrs ->
    if finishes instrs Nothing []
      then Just (snd $ last $ runInstructions instrs [] [])
      else part2 instructions (idx + 1)
  where
    newInstructions = replaceNthJmp instructions idx

tryReplacingNops instructions idx = case newInstructions of
  Nothing -> Nothing
  Just instrs ->
    if finishes instrs Nothing []
      then Just (snd $ last $ runInstructions instrs [] [])
      else part2 instructions (idx + 1)
  where
    newInstructions = replaceNthNop instructions idx

part2 instructions idx = case tryReplacingNops instructions idx of
  Nothing -> tryReplacingJmps instructions idx
  a -> a

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let rows = lines filecontents
      instructions = map parseInstr rows
      p1 = stateBeforeHalt instructions Nothing []
      p2 = part2 instructions 0
  putStrLn ("Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2)
