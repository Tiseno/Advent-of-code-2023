import qualified Data.Array      as Array
import qualified Data.List.Extra as List
import qualified Data.Map        as Map
import qualified Data.Maybe      as Maybe
import qualified Data.MultiMap   as MultiMap
import qualified Data.Ord        as Ord
import qualified Data.Set        as Set

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

part2 m = sum $ search Set.empty start [0]
  where
    start = (1, 0)
    bounds = Array.bounds m
    (xMax, yMax) = snd bounds
    destination = (xMax - 1, yMax)
    left (x, y) = (x - 1, y)
    right (x, y) = (x + 1, y)
    up (x, y) = (x, y - 1)
    down (x, y) = (x, y + 1)
    reachableNeighbors pos m =
      filter
        (\p ->
           Array.inRange bounds p && m Array.! p `List.notElem` ['S', 'O', '#'])
        [left pos, right pos, up pos, down pos]
    isNode m p = m Array.! p /= '#' && length (reachableNeighbors p m) > 2
    nodes =
      Set.insert destination $
      Set.fromList $ filter (isNode m) $ Array.indices m
    walkToNode (m0, edges) src (steps, current) =
      if Set.member current nodes
        then let newEdges = (List.sort [src, current], steps + 1) : edges
              in case reachableNeighbors current m0 of
                   [] -> (m0, newEdges)
                   reachable ->
                     foldl
                       (\s r -> walkToNode s current (0, r))
                       (m0, newEdges)
                       reachable
        else let m1 = m0 Array.// [(current, 'O')]
              in case filter (src /=) $ reachableNeighbors current m1 of
                   []     -> (m1, edges)
                   [next] -> walkToNode (m1, edges) src (steps + 1, next)
    withStart = m Array.// [(start, 'S')]
    distances =
      Map.fromList $ snd $ walkToNode (withStart, []) start (-1, start)
    edges =
      MultiMap.toMap $
      MultiMap.fromList $
      concatMap (\([a, b], _) -> [(a, b), (b, a)]) $ Map.toList distances
    search _ current dist
      | current == destination = dist
    search visited current dist =
      let nexts = Maybe.fromMaybe [] $ Map.lookup current edges
          unvisitedNexts = filter (`Set.notMember` visited) nexts
          paths =
            fmap
              (\next ->
                 search
                   (Set.insert current visited)
                   next
                   (dist ++ [distances Map.! List.sort [current, next]]))
              unvisitedNexts
       in if null paths
            then []
            else snd $
                 List.maximumBy (Ord.comparing fst) $
                 (\path -> (sum path, path)) <$> paths

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  r <- part1 parsed
  print r
  putStrLn "Part 2"
  print $ part2 parsed
