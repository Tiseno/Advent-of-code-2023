import qualified Data.Array    as Array
import qualified Data.Foldable as Foldable
import qualified Data.List     as List
import qualified Data.Map      as Map
import qualified Data.Maybe    as Maybe
import qualified Data.Ord      as Ord
import qualified Data.Set      as Set
import qualified Debug.Trace   as Debug

parseInput input =
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

type Tentative = Map.Map Node Int

type Tentative2 = [(Node, Int)]

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

solve minSteps maxSteps (cityMap :: Array.Array (Int, Int) Int) =
  snd $
  List.minimumBy (Ord.comparing snd) $
  filter (\((pos, _), _) -> pos == target) $ Map.toList minimumDistances
  where
    minimumDistances =
      dijkstra 0 Map.empty Set.empty [(((0, 0), NoDirection) :: Node, 0)]
    target = snd $ Array.bounds cityMap
    bigNumber = 999999999
    bounds = Array.bounds cityMap
    goWithDistance ((pos, dir), currentDist) =
      let newPos = go pos dir
       in ( (newPos, dir)
          , currentDist + Maybe.fromMaybe 0 (arrayLookup cityMap newPos))
    goSteps pos currentDist dir =
      drop minSteps $
      take (1 + maxSteps) $ iterate goWithDistance ((pos, dir), currentDist)
    reachableNeighbors count (visited :: Visited) currentDist ((currentPos, currentDir) :: Node) =
      let allowedDirections =
            filter (`notElem` [currentDir, opposite currentDir]) [R, L, D, U]
          threeSteps =
            concatMap (goSteps currentPos currentDist) allowedDirections
          notVisitedAndInBounds =
            filter
              (\(n@(pos, _), _) ->
                 n `Set.notMember` visited && Array.inRange bounds pos)
              threeSteps
       in notVisitedAndInBounds
    insertSmaller map (k, dist) =
      let prevDist = Maybe.fromMaybe bigNumber $ Map.lookup k map
       in Map.insert k (min dist prevDist) map
    dijkstra count (distances0 :: Distances) (visited0 :: Visited) (tentative0 :: Tentative2) =
      case tentative0 of
        [] -> distances0
        (currentNode, currentDist):tentative1 ->
          let distances1 = insertSmaller distances0 (currentNode, currentDist)
              visited1 = Set.insert currentNode visited0
              newTentativeNeighbors =
                reachableNeighbors count visited1 currentDist currentNode
              tentative2 = foldl qSetInsert tentative1 newTentativeNeighbors
           in dijkstra (count + 1) distances1 visited1 tentative2

main = do
  input <- readFile "input.txt"
  let cityMap = parseInput input
  putStrLn "Part 1"
  print $ solve 1 3 cityMap
  putStrLn "Part 2"
  print $ solve 4 10 cityMap
