import qualified Data.List   as List
import qualified Data.Set    as Set
import qualified Debug.Trace as Debug

expand input =
  unlines $
  List.transpose $
  foldl (\acc l -> acc ++ doubleIfAllDots l) [] $
  List.transpose $ foldl (\acc l -> acc ++ doubleIfAllDots l) [] $ lines input
  where
    doubleIfAllDots l
      | all (== '.') l = [l, l]
      | otherwise = [l]

positions expanded = foldl f [] withY
  where
    withY = zip [0 :: Int ..] $ lines expanded
    f acc (y, l) =
      let withX = zip [0 :: Int ..] l
          galaxyXs = fst <$> filter (\(_, c) -> c == '#') withX
          galaxyPositions = fmap (, y) galaxyXs
       in acc ++ galaxyPositions

part1 galaxyPositions = sum $ sum . snd <$> allDistances galaxyPositions
  where
    allDistances [_]    = []
    allDistances (g:gs) = (g, fmap (distance g) gs) : allDistances gs
    distance a@(x, y) b@(x', y') = abs (x' - x) + abs (y' - y)

findExpansions input = (xs, ys)
  where
    positionsOfAllDots i =
      fmap fst $ filter (\(y, l) -> all (== '.') l) $ zip [0 :: Int ..] i
    xs = positionsOfAllDots $ List.transpose $ lines input
    ys = positionsOfAllDots $ lines input

part2 expansionFactor (xExpansions, yExpansions) galaxyPositions =
  sum $ sum . snd <$> allDistances galaxyPositions
  where
    allDistances [_]    = []
    allDistances (g:gs) = (g, fmap (distance g) gs) : allDistances gs
    distance a@(x, y) b@(x', y') =
      let traversedXExpansions =
            filter (\e -> min x x' < e && e < max x x') xExpansions
          traversedYExpansions =
            filter (\e -> min y y' < e && e < max y y') yExpansions
          expandedDistance =
            (expansionFactor - 1) *
            (length traversedXExpansions + length traversedYExpansions)
       in expandedDistance + abs (x' - x) + abs (y' - y)

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  let expanded = expand input
  let expandedGalaxyPositions = positions expanded
  print $ part1 expandedGalaxyPositions
  putStrLn "Part 1"
  let expansionPositions = findExpansions input
  let galaxyPositions = positions input
  print $ part2 1000000 expansionPositions galaxyPositions
