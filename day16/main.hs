import qualified Data.Set as Set

parseInput = lines

data Dir
  = L
  | R
  | U
  | D
  deriving (Eq, Show, Ord)

part1 (mirrorMap :: [[Char]]) start =
  length $
  Set.toList $ Set.fromList $ fmap snd $ Set.toList $ energize Set.empty start
  where
    width = length $ head mirrorMap
    height = length mirrorMap
    go L (x, y) = (L, (x - 1, y))
    go R (x, y) = (R, (x + 1, y))
    go U (x, y) = (U, (x, y - 1))
    go D (x, y) = (D, (x, y + 1))
    energize energized0 current@(dir :: Dir, pos@(x :: Int, y :: Int))
      | x < 0 ||
          x >= width || y < 0 || y >= height || Set.member current energized0 =
        energized0
      | otherwise =
        let energized' = Set.insert current energized0
         in case (dir, mirrorMap !! y !! x) of
              (d, '|')
                | d `elem` [L, R] ->
                  energize (energize energized' (go U pos)) (go D pos)
              (d, '-')
                | d `elem` [U, D] ->
                  energize (energize energized' (go L pos)) (go R pos)
              (L, '\\') -> energize energized' (go U pos)
              (R, '\\') -> energize energized' (go D pos)
              (U, '\\') -> energize energized' (go L pos)
              (D, '\\') -> energize energized' (go R pos)
              (L, '/') -> energize energized' (go D pos)
              (R, '/') -> energize energized' (go U pos)
              (U, '/') -> energize energized' (go R pos)
              (D, '/') -> energize energized' (go L pos)
              _ -> energize energized' (go dir pos)

part2 mirrorMap = maximum $ part1 mirrorMap <$> starts
  where
    width = length $ head mirrorMap
    height = length mirrorMap
    startsLeft = [(R, (0, y)) | y <- [0 .. height - 1]]
    startsRight = [(L, (width - 1, y)) | y <- [0 .. height - 1]]
    startsTop = [(D, (x, 0)) | x <- [0 .. width - 1]]
    startsBot = [(U, (x, height - 1)) | x <- [0 .. width - 1]]
    starts = startsLeft ++ startsRight ++ startsTop ++ startsBot

main = do
  input <- readFile "input.txt"
  let mirrorMap = parseInput input
  putStrLn "Part 1"
  print $ part1 mirrorMap (R, (0, 0))
  putStrLn "Part 2"
  print $ part2 mirrorMap
