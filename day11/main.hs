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

part1 galaxyPositions = sum $ fmap (sum . snd) $ allDistances galaxyPositions
  where
    allDistances [_]    = []
    allDistances (g:gs) = (g, fmap (distance g) gs) : allDistances gs
    sumDistances (g:gs) = foldl (\ss b -> ss + distance g b) 0 gs
    distance a@(x, y) b@(x', y') = abs (x' - x) + abs (y' - y)

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  let expanded = expand input
  putStrLn expanded
  let galaxyPositions = positions expanded
  print $ part1 galaxyPositions
