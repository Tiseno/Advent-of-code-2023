import qualified Data.List.Split as Split
import qualified Data.List as List

parseInput input = lines <$> Split.splitOn "\n\n" input

isReflection reversedLeft right = all (uncurry (==)) $ zip reversedLeft right

findReflectionsOfPattern (p:: [[Char]]) =
  filter (\(_, (l, r)) -> isReflection l r)
  $ zip [1..] $ drop 1 $ init $ zip (reverse $ List.tails (reverse p)) (List.tails p)

reflection (pattern:: [[Char]]) =
  let p1 = pattern
      p2 = List.transpose pattern
      (ref1, ref2) = (findReflectionsOfPattern p1, findReflectionsOfPattern p2)
   in fmap ('H',) (fst <$> ref1) ++ fmap ('V',) (fst <$> ref2)

valueOf ('H', i) = i*100
valueOf ('V', i) = i

reflectionValue (pattern:: [[Char]]) = valueOf $ head $ reflection pattern

part1 input = sum $ reflectionValue <$> input

updateAtM :: (a -> a) -> (Int,Int) -> [[a]] -> [[a]]
updateAtM fn (x, y) m =
  let before = take y m
      after = drop (y + 1) m
      l = m List.!! y
      updated = updateAt fn x l
   in before ++ [updated] ++ after
  where
    updateAt fn i l =
      let before = take i l
          after = drop (i + 1) l
          e = l List.!! i
       in before ++ [fn e] ++ after

allFlippedSmudges :: [[Char]] -> [[[Char]]]
allFlippedSmudges pattern = fmap (flipSmudgeAt pattern) [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
  where
    height = length pattern
    width = length $ head pattern
    flip '#' = '.'
    flip '.' = '#'
    flipSmudgeAt p xy = updateAtM flip xy p

tryAllSmudges (pattern:: [[Char]]) =
  let previousReflection = head $ reflection pattern
      smudgeReflection = concatMap reflection (allFlippedSmudges pattern)
      newReflections = filter (/= previousReflection) smudgeReflection
   in valueOf $ head newReflections

part2 input = sum $ tryAllSmudges <$> input

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print  $ part1 parsed
  putStrLn "Part 2"
  print $ part2 parsed
