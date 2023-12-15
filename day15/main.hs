import qualified Data.Char       as Char
import qualified Data.List.Split as Split

parseInput input = Split.splitOn "," $ head $ lines input

part1 input = sum $ fmap (hhash 0) input
  where
    hhash current [] = current
    hhash (current :: Int) (x:xs) =
      hhash (((current + Char.ord x) * 17) `rem` 256) xs

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
