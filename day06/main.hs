import qualified Data.List.Split as Split

parseInput1 :: String -> [(Int, Int)]
parseInput1 input =
  let [[_, times], [_, distances]] = Split.splitOn ":" <$> lines input
   in zip (read <$> words times) (read <$> words distances)

-- Naive version try all x's = [0..time]
part1 races = product $ fmap (length . waysToBeat) races
  where
    waysToBeat (time, dist) =
      filter (> dist) $ (\x -> x * (time - x)) <$> [0 .. time]

parseInput2 :: String -> (Int, Int)
parseInput2 input =
  let [[_, times], [_, distances]] = Split.splitOn ":" <$> lines input
   in (read $ concat $ words times, read $ concat $ words distances)

-- Better version, find the smallest x and the biggest x with binary search
part2 (time, dist) = 1 + findBound time time (-1) - findBound 0 time 1
  where
    raceFormula x = x * (time - x)
    findBound x p d =
      let p' = max 1 (p `div` 2)
       in if raceFormula x < dist && raceFormula (x + d) >= dist
            then x + d
            else if raceFormula x < dist
                   then findBound (x + (d * p')) p' d
                   else findBound (x - (d * p')) p' d

main = do
  input <- readFile "input.txt"
  let parsed = parseInput1 input
  putStrLn "Part 1"
  print $ part1 parsed
  let parsed = parseInput2 input
  putStrLn "Part 2"
  print $ part2 parsed
