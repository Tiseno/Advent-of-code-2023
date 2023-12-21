import qualified Data.Array      as Array
import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Maybe      as Maybe
import qualified Data.Set        as Set

parseInput input =
  Array.array
    ((0, 0), (pred $ length (head l), pred $ length l))
    [((x, y), e) | (y, row) <- zip [0 ..] l, (x, e) <- zip [0 ..] row]
  where
    l = lines input

printMatrix m =
  let (_, (xMax, _)) = Array.bounds m
   in mapM_ putStrLn $ Split.chunksOf (xMax + 1) $ Array.elems m

part1 maxSteps (gardenMap :: Array.Array (Int, Int) Char) =
  go 0 (Set.singleton startPosition)
  where
    startPosition =
      fst $
      Maybe.fromJust $ List.find (\(i, e) -> e == 'S') $ Array.assocs gardenMap
    bounds = Array.bounds gardenMap
    reachableNeighbors (x :: Int, y :: Int) =
      filter
        (\p -> Array.inRange bounds p && gardenMap Array.! p /= '#')
        [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    go currentStep (currents :: Set.Set (Int, Int))
      | currentStep == maxSteps = currents
    go currentStep (currents :: Set.Set (Int, Int)) =
      let reachable =
            Set.fromList $ concatMap reachableNeighbors (Set.elems currents)
       in go (currentStep + 1) reachable

main = do
  putStrLn "Part 1"
  input <- readFile "input.txt"
  let gardenMap = parseInput input
  print $ length $ part1 64 gardenMap
