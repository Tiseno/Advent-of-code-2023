import qualified Data.Char     as Char
import qualified Data.MultiMap as MultiMap

partInVicinityOfRange input (x0, x1) y0 =
  any (\c -> not (Char.isDigit c) && c /= '.') $
  concat $ concatMap f $ zip [0 ..] $ lines input
  where
    start = x0 - 1
    end = x1 + 1
    f (y, row) = [getXRange row | y >= y0 - 1 && y <= y0 + 1]
    getXRange row = take (end - start) $ drop (max 0 start) row

parseInput1 :: String -> (Int, Int) -> String -> [Int]
parseInput1 input (x, y) ('\n':as) = parseInput1 input (0, y + 1) as
parseInput1 input (x, y) as@(a:_)
  | Char.isDigit a =
    let (s, as') = span Char.isDigit as
        n = read s
        isPartNumber = partInVicinityOfRange input (x, x + length s) y
     in [n | isPartNumber] ++ parseInput1 input (x + length s, y) as'
parseInput1 input (x, y) (_:as) = parseInput1 input (x + 1, y) as
parseInput1 _ _ [] = []

part1 input = sum $ parseInput1 input (0, 0) input

starsInVicinityOfRange :: String -> (Int, Int) -> Int -> [(Int, Int)]
starsInVicinityOfRange input (x0, x1) y0 =
  fmap fst $
  filter (\(_, b) -> b == '*') $ concat $ concatMap f $ zip [0 ..] $ lines input
  where
    start = x0 - 1
    end = x1 + 1
    f (y, row) = [getXRange y row | y >= y0 - 1 && y <= y0 + 1]
    getXRange y row =
      take (end - start) $ drop (max 0 start) $ zip (fmap (, y) [0 ..]) row

parseInput2 ::
     String
  -> MultiMap.MultiMap (Int, Int) Int
  -> (Int, Int)
  -> String
  -> MultiMap.MultiMap (Int, Int) Int
parseInput2 input starMap (x, y) ('\n':as) =
  parseInput2 input starMap (0, y + 1) as
parseInput2 input starMap (x, y) as@(a:_)
  | Char.isDigit a =
    let (s, as') = span Char.isDigit as
        n = read s
        adjacentStarPositions = starsInVicinityOfRange input (x, x + length s) y
        starMap' =
          foldl (\m s -> MultiMap.insert s n m) starMap adjacentStarPositions
     in parseInput2 input starMap' (x + length s, y) as'
parseInput2 input starMap (x, y) (_:as) =
  parseInput2 input starMap (x + 1, y) as
parseInput2 _ starMap _ [] = starMap

part2 input =
  sum $
  fmap (\(_, b) -> product b) $
  filter (\(_, b) -> length b >= 2) $
  MultiMap.assocs $ parseInput2 input MultiMap.empty (0, 0) input

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
