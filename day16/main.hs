import qualified Data.Set as Set

parseInput = lines

data Dir
  = L
  | R
  | U
  | D
  deriving (Eq, Show, Ord)

go L (x, y) = (L, (x - 1, y))
go R (x, y) = (R, (x + 1, y))
go U (x, y) = (U, (x, y - 1))
go D (x, y) = (D, (x, y + 1))

part1 (mirrorMap :: [[Char]]) =
  length $
  Set.toList $
  Set.fromList $ fmap snd $ Set.toList $ energize Set.empty (R, (0, 0))
  where
    width = length $ head mirrorMap
    height = length mirrorMap
    energize energized0 current@(dir :: Dir, pos@(x :: Int, y :: Int))
      | x < 0 ||
          x >= width || y < 0 || y >= height || Set.member current energized0 =
        energized0
      | otherwise =
        let energized' = Set.insert current energized0
         in case (dir, mirrorMap !! y !! x) of
              (d, '|')
                | d == L || d == R ->
                  let energize'' = energize energized' (go U pos)
                   in energize energize'' (go D pos)
              (d, '-')
                | d == U || d == D ->
                  let energize'' = energize energized' (go L pos)
                   in energize energize'' (go R pos)
              (L, '\\') -> energize energized' (go U pos)
              (R, '\\') -> energize energized' (go D pos)
              (U, '\\') -> energize energized' (go L pos)
              (D, '\\') -> energize energized' (go R pos)
              (L, '/') -> energize energized' (go D pos)
              (R, '/') -> energize energized' (go U pos)
              (U, '/') -> energize energized' (go R pos)
              (D, '/') -> energize energized' (go L pos)
              _ -> energize energized' (go dir pos)

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
