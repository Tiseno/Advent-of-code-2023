import qualified Data.Set    as Set
import qualified Debug.Trace as Debug
import qualified Numeric

data Dir
  = L
  | R
  | U
  | D
  deriving (Show, Read, Eq, Ord)

data DigInstruction =
  DI
    { dir   :: Dir
    , len   :: Integer
    , color :: String
    }
  deriving (Read, Eq, Ord)

instance Show DigInstruction where
  show DI {dir, len, color} = show dir ++ " " ++ show len ++ " " ++ color

parseInput1 input = parseLine <$> lines input
  where
    parseLine line =
      let [dir, len, color] = words line
       in DI (read dir) (read len) color

go pos dir len = go' pos dir <$> [1 .. len]
  where
    go' (x, y) L len = (x - len, y)
    go' (x, y) R len = (x + len, y)
    go' (x, y) U len = (x, y - len)
    go' (x, y) D len = (x, y + len)

type Pos = (Integer, Integer)

part1 (digPlan :: [DigInstruction]) =
  length trenches +
  length
    (interiorDig
       Set.empty
       (Set.fromList [interiorStart1])
       Set.empty
       (Set.fromList [interiorStart2]))
  where
    start = (0, 0)
    dig [] (pos :: (Integer, Integer)) = []
    dig (DI {dir, len}:instrs) pos =
      let newTrench = go pos dir len
       in newTrench ++ dig instrs (last newTrench)
    trenches = Set.fromList $ dig digPlan start
    -- Start interior dig above and under the first trench
    (interiorStart1, interiorStart2) =
      let [(x, y)] = go start (dir $ head digPlan) 1
       in ((x, y + 1), (x, y - 1))
    -- Start a dig in two places, choose the interior that finishes first
    -- Assume trenches are separated by one space, otherwise its game over
    reachableNeighbors interior (x, y) =
      filter
        (\p -> p `Set.notMember` trenches && p `Set.notMember` interior)
        [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    interiorDig (visited1 :: Set.Set Pos) (pending1 :: Set.Set Pos) (visited2 :: Set.Set Pos) (pending2 :: Set.Set Pos) =
      case (Set.minView pending1, Set.minView pending2) of
        (Nothing, _) -> visited1
        (_, Nothing) -> visited2
        (Just (p1, pending1'), Just (p2, pending2')) ->
          let reachable1 = reachableNeighbors visited1 p1
              reachable2 = reachableNeighbors visited2 p2
           in interiorDig
                (Set.insert p1 visited1)
                (foldl (flip Set.insert) pending1' reachable1)
                (Set.insert p2 visited2)
                (foldl (flip Set.insert) pending2' reachable2)

convertToBigPlan digPlan = convertToRealBigDigInstr <$> digPlan
  where
    hexDir '0' = R
    hexDir '1' = D
    hexDir '2' = L
    hexDir '3' = U
    convertToRealBigDigInstr DI {color} =
      let dir = hexDir $ color !! 7
          lens = Numeric.readHex $ drop 1 $ take 6 $ tail color
       in DI dir (fst $ head lens) color

part2 (bigPlan :: [DigInstruction]) = snd shapeArea + pathLength `div` 2 + 1
  where
    pathLength = sum $ len <$> bigPlan
    shapeArea = foldl sumAreas (0, 0) bigPlan
    sumAreas (y :: Integer, area) DI {dir, len} =
      case dir of
        L -> (y, area - y * len)
        R -> (y, area + y * len)
        U -> (y + len, area)
        D -> (y - len, area)

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  let digPlan = parseInput1 input
  print $ part1 digPlan
  putStrLn "Part 2"
  let bigPlan = convertToBigPlan digPlan
  print $ part2 bigPlan
