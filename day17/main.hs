import qualified Data.Array as Array
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

parseCityMap input =
  (\c -> read [c] :: Int) <$>
  Array.array
    ((0, 0), (pred $ length (head l), pred $ length l))
    [((x, y), e) | (y, row) <- zip [0 ..] l, (x, e) <- zip [0 ..] row]
  where
    l = lines input

data Dir
  = NoDirection -- Used for the start node, from this node we can go in all directions
  | L
  | R
  | U
  | D
  deriving (Eq, Show, Ord)

opposite :: Dir -> Dir
opposite NoDirection = NoDirection
opposite L           = R
opposite R           = L
opposite U           = D
opposite D           = U

type Pos = (Int, Int)

type Node = (Pos, Dir)

type Visited = Set.Set Node

type Distances = Map.Map Node Int

type Tentative = [(Node, Int)]

qSetInsert [] (k, v) = [(k, v)]
qSetInsert ((k0, v0):stack) (k, v)
  | k0 == k =
    if v0 < v
      then (k0, v0) : stack
      else qSetInsert stack (k, v)
  | v0 < v = (k0, v0) : qSetInsert stack (k, v)
  | otherwise = (k, v) : (k0, v0) : remove k stack
  where
    remove _ [] = []
    remove k ((k0, v0):stack)
      | k == k0 = stack
      | otherwise = (k0, v0) : remove k stack

arrayLookup a i =
  if Array.inRange (Array.bounds a) i
    then Just $ a Array.! i
    else Nothing

go (x, y) R = (x + 1, y)
go (x, y) L = (x - 1, y)
go (x, y) U = (x, y - 1)
go (x, y) D = (x, y + 1)

leastHeatLoss minSteps maxSteps (cityMap :: Array.Array (Int, Int) Int) =
  minimum $
  snd <$> filter (\((pos, _), _) -> pos == target) (Map.toList minimumDistances)
  where
    start = (0, 0)
    bounds = Array.bounds cityMap
    target = snd bounds
    minimumDistances = dijkstra Map.empty Set.empty [((start, NoDirection), 0)]
    bigNumber = 999999999
    goWithDistance ((pos, dir), currentDist) =
      let newPos = go pos dir
       in ( (newPos, dir)
          , currentDist + Maybe.fromMaybe 0 (arrayLookup cityMap newPos))
    goSteps pos currentDist dir =
      drop minSteps $
      take (1 + maxSteps) $ iterate goWithDistance ((pos, dir), currentDist)
    reachableNeighbors (visited :: Visited) currentDist ((currentPos, currentDir) :: Node) =
      let allowedDirections =
            filter (`notElem` [currentDir, opposite currentDir]) [R, L, D, U]
       in filter
            (\(n@(pos, _), _) ->
               n `Set.notMember` visited && Array.inRange bounds pos)
            (concatMap (goSteps currentPos currentDist) allowedDirections)
    insertSmaller k v map =
      let prevV = Maybe.fromMaybe bigNumber $ Map.lookup k map
       in Map.insert k (min v prevV) map
    dijkstra :: Distances -> Visited -> Tentative -> Distances
    dijkstra distances _ [] = distances
    dijkstra distances0 visited0 ((currentNode, currentDist):tentative0) =
      let distances1 = insertSmaller currentNode currentDist distances0
          visited1 = Set.insert currentNode visited0
          newTentativeNeighbors =
            reachableNeighbors visited1 currentDist currentNode
          tentative1 = foldl qSetInsert tentative0 newTentativeNeighbors
       in dijkstra distances1 visited1 tentative1

main = do
  cityMap <- parseCityMap <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ leastHeatLoss 1 3 cityMap
  putStrLn "Part 2"
  print $ leastHeatLoss 4 10 cityMap
