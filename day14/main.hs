import qualified Data.Hashable   as Hashable
import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Map        as Map

pushNorth platform = List.transpose $ pushWest $ List.transpose platform

pushWest = fmap rowPushWest
  where
    rowPushWest row =
      concatMap pushWestInSubRow (Split.split (Split.whenElt (== '#')) row)
    pushWestInSubRow [] = []
    pushWestInSubRow ['#'] = ['#']
    pushWestInSubRow row = reverse $ List.sort row

totalLoad platform =
  sum $
  fmap (uncurry (*)) $
  zip [1 ..] $ reverse $ fmap (length . filter (== 'O')) platform

part1 platform = totalLoad $ pushNorth platform

pushEast platform = fmap reverse $ pushWest $ fmap reverse platform

pushSouth platform = List.transpose $ pushEast $ List.transpose platform

spin platform = pushEast $ pushSouth $ pushWest $ pushNorth platform

part2 platform0 =
  let initialIterations = 500
      platformI = (!! initialIterations) $ iterate spin platform0
      (h1, platform1) = oneSpinWithHash platformI
      (h2, platform2) = oneSpinWithHash platform1
      (h3, platform3) = oneSpinWithHash platform2
      (h4, platform4) = oneSpinWithHash platform3
      (h5, platform5) = oneSpinWithHash platform4
      (h6, platform6) = oneSpinWithHash platform5
      (h7, platform7) = oneSpinWithHash platform6
      (h8, platform8) = oneSpinWithHash platform7
      (h9, platform9) = oneSpinWithHash platform8
      (h10, platform10) = oneSpinWithHash platform9
      pattern = (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10)
      initialCount = initialIterations + 10
      (cycleStart, current, platformN) = findCycle initialCount (Map.fromList [(pattern, initialCount)]) pattern platform10
      target = 1000000000
      cycleLength = current - cycleStart
      neededCycles = (target - cycleStart) `div` cycleLength
      missingSpins = target - (cycleStart + cycleLength * neededCycles)
   in totalLoad $ (!! missingSpins) $ iterate spin platformN
  where
    findCycle count cycles (_, h1, h2, h3, h4, h5, h6, h7, h8, h9) platform =
      let (h10, platform10) = oneSpinWithHash platform
          newPattern = (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10)
          newCount = count + 1
       in case Map.lookup newPattern cycles of
            Just cycleStart -> (cycleStart, newCount, platform10)
            Nothing ->
              findCycle
                newCount
                (Map.insert newPattern newCount cycles)
                newPattern
                platform10
    oneSpinWithHash platform =
      let north = pushNorth platform
          west = pushWest north
          south = pushSouth west
          east = pushEast south
          cycleHash = Hashable.hash (north, west, south, east)
       in (cycleHash, east)

main = do
  input <- readFile "input.txt"
  let parsed = lines input
  putStrLn "Part 1"
  print $ part1 parsed
  putStrLn "Part 2"
  print $ part2 parsed
