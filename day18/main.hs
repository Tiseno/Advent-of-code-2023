import qualified Data.Set as Set

data Dir
  = L
  | R
  | U
  | D
  deriving (Show, Read, Eq, Ord)

data DigInstruction =
  DI
    { dir   :: Dir
    , len   :: Int
    , color :: String
    }
  deriving (Read, Eq, Ord)

instance Show DigInstruction where
  show DI {dir, len, color} = show dir ++ " " ++ show len ++ " " ++ color

parseInput input = parseLine <$> lines input
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

type Pos = (Int, Int)

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
    dig [] (pos :: (Int, Int)) = []
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

main = do
  input <- readFile "input.txt"
  let digPlan = parseInput input
  putStrLn "Part 1"
  print $ part1 digPlan
