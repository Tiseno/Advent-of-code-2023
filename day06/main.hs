import qualified Data.List.Split as Split

parseInput :: String -> [(Int, Int)]
parseInput input =
  let [[_, times], [_, distances]] = Split.splitOn ":" <$> lines input
   in zip (read <$> words times) (read <$> words distances)

-- Naive version try all x's = [0..time]
part1 races = product $ fmap (length . waysToBeat) races
  where
    waysToBeat (time, dist) =
      filter (> dist) $ (\x -> x * (time - x)) <$> [0 .. time]

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
