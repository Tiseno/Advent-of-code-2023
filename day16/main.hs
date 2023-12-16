import qualified Data.Array as Array
import qualified Data.Set   as Set

parseInput input =
  Array.array
    ((0, 0), (pred $ length (head l), pred $ length l))
    [((x, y), e) | (y, row) <- zip [0 ..] l, (x, e) <- zip [0 ..] row]
  where
    l = lines input

data Dir
  = L
  | R
  | U
  | D
  deriving (Eq, Show, Ord)

part1 mirrorMap start =
  length $
  Set.toList $ Set.fromList $ fmap snd $ Set.toList $ energize Set.empty start
  where
    bounds = Array.bounds mirrorMap
    go L (x, y) = (L, (pred x, y))
    go R (x, y) = (R, (succ x, y))
    go U (x, y) = (U, (x, pred y))
    go D (x, y) = (D, (x, succ y))
    energize energized0 (dir, pos)
      | not (Array.inRange bounds pos) || Set.member (dir, pos) energized0 =
        energized0
      | otherwise =
        let energized' = Set.insert (dir, pos) energized0
         in case (dir, mirrorMap Array.! pos) of
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
    (_, (xMax, yMax)) = Array.bounds mirrorMap
    startsLeft = [(R, (0, y)) | y <- [0 .. yMax]]
    startsRight = [(L, (xMax, y)) | y <- [0 .. yMax]]
    startsTop = [(D, (x, 0)) | x <- [0 .. xMax]]
    startsBot = [(U, (x, yMax)) | x <- [0 .. xMax]]
    starts = startsLeft ++ startsRight ++ startsTop ++ startsBot

main = do
  input <- readFile "input.txt"
  let mirrorMap = parseInput input
  putStrLn "Part 1"
  print $ part1 mirrorMap (R, (0, 0))
  putStrLn "Part 2"
  print $ part2 mirrorMap
