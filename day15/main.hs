import qualified Data.Char       as Char
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import qualified Data.Maybe      as Maybe

parseInput input = Split.splitOn "," $ head $ lines input

hhash = hhash' 0
  where
    hhash' current [] = current
    hhash' (current :: Int) (x:xs) =
      hhash' (((current + Char.ord x) * 17) `rem` 256) xs

part1 input = sum $ fmap hhash input

part2 steps = sum $ focusingPower <$> Map.toList lensBoxes
  where
    focusingPower (i, lenses) =
      sum $ (\(j, (_, fl)) -> (i + 1) * j * fl) <$> zip [1 ..] lenses
    lensBoxes = foldl insertIntoBoxes Map.empty steps
    insertOrReplace a [] = [a]
    insertOrReplace a@(l, _) ((x@(label, _):xs))
      | l == label = a : xs
      | otherwise = x : insertOrReplace a xs
    insertIntoBoxes boxes step =
      let (label, operation:f) = span Char.isAlpha step
          h = hhash label
          boxContent = Maybe.fromMaybe [] $ Map.lookup h boxes
          newContent =
            case operation of
              '-' -> filter (\(l, _) -> l /= label) boxContent
              '=' -> insertOrReplace (label, read f) boxContent
       in Map.insert h newContent boxes

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
  putStrLn "Part 2"
  print $ part2 parsed
