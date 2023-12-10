import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map       as Map
import qualified Data.Maybe     as Maybe
import qualified Data.Set       as Set
import qualified Debug.Trace    as Debug

parseInput input =
  Bifunctor.first Maybe.fromJust $
  foldl parseLine (Nothing, Map.empty) linesWithY
  where
    linesWithY = zip [0 ..] $ lines input
    parseLine acc (y :: Int, line) = foldl (parseCell y) acc $ zip [0 ..] line
    connectionsFor (x, y) '-' = [(x - 1, y), (x + 1, y)]
    connectionsFor (x, y) '|' = [(x, y - 1), (x, y + 1)]
    connectionsFor (x, y) '7' = [(x - 1, y), (x, y + 1)]
    connectionsFor (x, y) 'J' = [(x, y - 1), (x - 1, y)]
    connectionsFor (x, y) 'L' = [(x + 1, y), (x, y - 1)]
    connectionsFor (x, y) 'F' = [(x, y + 1), (x + 1, y)]
    parseCell y (s, map) (x :: Int, c) =
      let s' =
            if c == 'S'
              then Just (x, y)
              else s
          map' =
            if c /= '.' && c /= 'S'
              then Map.insert (x, y) (connectionsFor (x, y) c) map
              else map
       in (s', map')

part1 ((s@(x, y), map) :: ((Int, Int), Map.Map (Int, Int) [(Int, Int)])) =
  snd (walk (Set.fromList [n0]) 2 n1) `div` 2
  where
    neighbors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    neighborsPipes =
      Maybe.mapMaybe (\n -> fmap (n, ) $ n `Map.lookup` map) neighbors
    n0 = fst $ head $ filter (\(n, cs) -> s `elem` cs) neighborsPipes
    n1 = head $ filter (s /=) $ map Map.! n0
    walk visited count current
      | current == s = (current, count)
      | current `Set.member` visited = (current, count)
      | otherwise =
        let unvisited = filter (`Set.notMember` visited) $ map Map.! current
         in if null unvisited
              then (current, count)
              else walk
                     (Set.insert current visited)
                     (count + 1)
                     (head unvisited)

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
