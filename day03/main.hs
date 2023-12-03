import qualified Data.Bifunctor  as Bifunctor
import qualified Data.Char       as Char
import qualified Data.List.Split as Split
import qualified Debug.Trace     as Debug

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

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 input
