import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import qualified Data.MultiMap   as MultiMap
import qualified Data.Set        as Set

parseEdges input =
  MultiMap.toMap $ MultiMap.fromList $ concatMap parseLine (lines input)
  where
    parseLine l =
      let [a, targets] = Split.splitOn ": " l
       in concatMap (\target -> [(a, target), (target, a)]) (words targets)

part1 (edges :: Map.Map String [String]) start removed = visited * notVisited
  where
    total = length edges
    visited = length $ visitAllConnected Set.empty start
    notVisited = total - visited
    visitAllConnected visited0 current =
      let visited1 = Set.insert current visited0
          neighbors =
            filter (\n -> n `Set.notMember` visited1 && n `notElem` removed) $
            edges Map.! current
       in foldl visitAllConnected visited1 neighbors

main = do
  putStrLn "Part 1"
  edges <- parseEdges <$> readFile "input.txt"
  print $ part1 edges "pvs" ["kgl", "qfb", "xxq"]
