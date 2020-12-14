import Data.Char
import qualified Data.Map as Map

type Memory = Map.Map Int Int

type Sector = (String, [(Int, Int)])

bitVals = reverse $ takeWhile (<= (2 ^ 35)) $ iterate (* 2) 1

numTob36Bin [] _ acc = acc
numTob36Bin vals n acc =
  if n >= bitVal
    then numTob36Bin (tail vals) (n - bitVal) (acc ++ ['1'])
    else numTob36Bin (tail vals) n (acc ++ ['0'])
  where
    bitVal = head vals

numToBin :: Int -> String
numToBin n = numTob36Bin bitVals n ""

binToNum :: String -> Int
binToNum s = sum $ zipWith (*) bitVals binNums
  where
    binNums = map digitToInt s

applyMask :: String -> String -> String
applyMask = zipWith maskBit
  where
    maskBit mB bC = if mB == 'X' then bC else mB

applyFloatingMask :: String -> String -> String
applyFloatingMask = zipWith maskBit
  where
    maskBit mB bC = case mB of
      '0' -> bC
      a -> a

numAfterMask :: String -> Int -> Int
numAfterMask mask = binToNum . applyMask mask . numToBin

writeToMemory :: Int -> Int -> Memory -> Memory
writeToMemory = Map.insert

isMask :: String -> Bool
isMask s = (==) "mask" $ take 4 s

extractParts :: String -> (String, String)
extractParts s = (init $ takeWhile (/= '=') s, drop 2 $ dropWhile (/= '=') s)

extractInstruction :: String -> (Int, Int)
extractInstruction s = (adr, read val)
  where
    (addrI, val) = extractParts s
    adr = read $ filter isDigit addrI

takeSectors :: [String] -> [Sector] -> [Sector]
takeSectors [] acc = acc
takeSectors (x : xs) acc = takeSectors remainingItems (acc ++ [(mask, instructions)])
  where
    mask = snd $ extractParts x
    instructions = map extractInstruction $ takeWhile (not . isMask) xs
    remainingItems = dropWhile (not . isMask) xs

runSector :: Memory -> Sector -> Memory
runSector mem (mask, instrs) = foldr (uncurry writeToMemory) mem maskedInstructions
  where
    maskedInstructions = reverse $ map (\(idx, val) -> (idx, numAfterMask mask val)) instrs

writeMaskedToMemory :: String -> Int -> Int -> Memory -> Memory
writeMaskedToMemory mask pos val mem = foldr (uncurry Map.insert) mem arr
  where
    arr = zip positions (repeat val)
    positions = map binToNum $ binPerms $ applyFloatingMask mask $ numToBin pos

runSector2 :: Memory -> Sector -> Memory
runSector2 mem (mask, instrs) = foldr (uncurry (writeMaskedToMemory mask)) mem maskedInstructions
  where
    maskedInstructions = reverse instrs

solveP1 :: [String] -> Int
solveP1 rows = sum $ Map.elems $ foldl runSector Map.empty sectors
  where
    sectors = takeSectors rows []

binPerms :: String -> [String]
binPerms s =
  if ini == s
    then [s]
    else binPerms oneP ++ binPerms zeroP
  where
    ini = takeWhile (/= 'X') s
    rest = drop 1 $ dropWhile (/= 'X') s
    oneP = ini ++ ['1'] ++ rest
    zeroP = ini ++ ['0'] ++ rest

solveP2 :: [String] -> Int
solveP2 rows = sum $ Map.elems $ foldl runSector2 Map.empty sectors
  where
    sectors = takeSectors rows []

main :: IO ()
main = do
  filecontents <- readFile "input.txt"
  let rows = lines filecontents
      part1 = solveP1 rows
      part2 = solveP2 rows
  putStrLn ("Part 1: " ++ show part1 ++ "\n" ++ "Part 2: " ++ show part2)
