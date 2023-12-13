import qualified Data.List.Split as Split
import qualified Data.List as List

parseInput input = lines <$> Split.splitOn "\n\n" input

part1 input = sum $ reflectionValue <$> input

reflectionValue (pattern:: [[Char]]) =
  let p1 = pattern
      p2 = List.transpose pattern
      (ref1, ref2) = (findReflectionsOfPattern p1, findReflectionsOfPattern p2)
   in sum ((100 *) . fst <$> ref1) + sum (fst <$> ref2)


findReflectionsOfPattern (p:: [[Char]]) =
  filter (\(_, (l, r)) -> isReflection l r)
  $ zip [1..] $ drop 1 $ init $ zip (reverse $ List.tails (reverse p)) (List.tails p)

isReflection left right = all (uncurry (==)) $ zip left right

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print  $ part1 parsed
