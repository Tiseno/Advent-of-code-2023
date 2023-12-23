import qualified Data.Array      as Array
import qualified Data.List.Extra as List

parseInput input =
  Array.array
    ((0, 0), (pred $ length (head l), pred $ length l))
    [((x, y), e) | (y, row) <- zip [0 ..] l, (x, e) <- zip [0 ..] row]
  where
    l = lines input

printM m = do
  let width = 1 + fst (snd $ Array.bounds m)
  mapM_ putStrLn $ List.transpose $ List.chunksOf width $ Array.elems m

part1 m = do
  r <- step (1, 0) (0, withStart)
  pure $ maximum r
  where
    withStart = m Array.// [((1, 0), 'S')]
    left (x, y) = (x - 1, y)
    right (x, y) = (x + 1, y)
    up (x, y) = (x, y - 1)
    down (x, y) = (x, y + 1)
    inRangeAndNot m c p =
      Array.inRange (Array.bounds m) p &&
      m Array.! p `List.notElem` (c : ['S', 'O', '#'])
    reachableNeighbors p m =
      let ll = filter (inRangeAndNot m '>') [left p]
          rr = filter (inRangeAndNot m '<') [right p]
          uu = filter (inRangeAndNot m 'v') [up p]
          dd = filter (inRangeAndNot m '^') [down p]
       in concat [ll, rr, uu, dd]
    step p (steps :: Int, m) =
      case reachableNeighbors p m of
        [] -> pure [steps]
        [r] -> step r (steps + 1, m Array.// [(r, 'O')])
        reachable -> do
          se <- mapM (\r -> step r (steps + 1, m Array.// [(r, 'O')])) reachable
          pure (concat se)

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  r <- part1 parsed
  print r
