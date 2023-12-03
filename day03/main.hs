import qualified Data.Char     as Char
import qualified Data.MultiMap as MultiMap

inVicinityOfRange ::
     (Char -> Bool) -> [(Int, String)] -> (Int, Int) -> Int -> [(Int, Int)]
inVicinityOfRange pred indexedLines (x0, x1) y0 =
  fmap fst $ filter (pred . snd) $ concat $ concatMap f indexedLines
  where
    start = x0 - 1
    end = x1 + 1
    f (y, row) = [getXRange y row | y >= y0 - 1 && y <= y0 + 1]
    getXRange y row =
      take (end - start) $ drop (max 0 start) $ zip (fmap (, y) [0 ..]) row

parseInput1 :: [(Int, String)] -> (Int, Int) -> String -> [Int]
parseInput1 indexedLines (x, y) ('\n':as) =
  parseInput1 indexedLines (0, y + 1) as
parseInput1 indexedLines (x, y) as@(a:_)
  | Char.isDigit a =
    let (s, as') = span Char.isDigit as
        n = read s
        adjacentPartPositions =
          inVicinityOfRange
            (\c -> not (Char.isDigit c) && c /= '.')
            indexedLines
            (x, x + length s)
            y
        isPartNumber = not (null adjacentPartPositions)
     in [n | isPartNumber] ++ parseInput1 indexedLines (x + length s, y) as'
parseInput1 indexedLines (x, y) (_:as) = parseInput1 indexedLines (x + 1, y) as
parseInput1 _ _ [] = []

part1 (input :: String) =
  let indexedLines = zip [0 ..] (lines input)
   in sum $ parseInput1 indexedLines (0, 0) input

parseInput2 ::
     [(Int, String)]
  -> MultiMap.MultiMap (Int, Int) Int
  -> (Int, Int)
  -> String
  -> MultiMap.MultiMap (Int, Int) Int
parseInput2 indexedLines starMap (x, y) ('\n':as) =
  parseInput2 indexedLines starMap (0, y + 1) as
parseInput2 indexedLines starMap (x, y) as@(a:_)
  | Char.isDigit a =
    let (s, as') = span Char.isDigit as
        n = read s
        adjacentStarPositions =
          inVicinityOfRange (== '*') indexedLines (x, x + length s) y
        starMap' =
          foldl (\m s -> MultiMap.insert s n m) starMap adjacentStarPositions
     in parseInput2 indexedLines starMap' (x + length s, y) as'
parseInput2 indexedLines starMap (x, y) (_:as) =
  parseInput2 indexedLines starMap (x + 1, y) as
parseInput2 _ starMap _ [] = starMap

part2 input =
  let indexedLines = zip [0 ..] (lines input)
   in sum $
      fmap (\(_, b) -> product b) $
      filter (\(_, b) -> length b >= 2) $
      MultiMap.assocs $ parseInput2 indexedLines MultiMap.empty (0, 0) input

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
