import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.MultiSet   as MultiSet

parseInput = lines

pushNorth platform = List.transpose $ pushWest $ List.transpose platform
  where
    pushWest = fmap rowPushWest
    rowPushWest row =
      concatMap pushWestInSubRow (Split.split (Split.whenElt (== '#')) row)
    pushWestInSubRow [] = []
    pushWestInSubRow ['#'] = ['#']
    pushWestInSubRow row =
      let counts = MultiSet.fromList row
       in replicate (MultiSet.occur 'O' counts) 'O' ++
          replicate (MultiSet.occur '.' counts) '.'

totalLoad platform =
  sum $
  fmap (uncurry (*)) $
  zip [1 ..] $ reverse $ fmap (length . filter (== 'O')) platform

part1 platform = totalLoad $ pushNorth platform

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
