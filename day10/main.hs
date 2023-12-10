import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map       as Map
import qualified Data.Maybe     as Maybe
import qualified Data.Set       as Set
import qualified Debug.Trace    as Debug

parseInput input =
  Bifunctor.first Maybe.fromJust $
  foldl parseLine (Nothing, Map.empty) linesWithY
  where
    linesWithY = zip [0 ..] $ lines input
    parseLine acc (y :: Int, line) = foldl (parseCell y) acc $ zip [0 ..] line
    connectionsFor (x, y) '-' = [(x - 1, y), (x + 1, y)]
    connectionsFor (x, y) '|' = [(x, y - 1), (x, y + 1)]
    connectionsFor (x, y) '7' = [(x - 1, y), (x, y + 1)]
    connectionsFor (x, y) 'J' = [(x, y - 1), (x - 1, y)]
    connectionsFor (x, y) 'L' = [(x + 1, y), (x, y - 1)]
    connectionsFor (x, y) 'F' = [(x, y + 1), (x + 1, y)]
    parseCell y (s, map) (x :: Int, c) =
      let s' =
            if c == 'S'
              then Just (x, y)
              else s
          map' =
            if c /= '.' && c /= 'S'
              then Map.insert (x, y) (connectionsFor (x, y) c) map
              else map
       in (s', map')

findPath ((s@(x, y), map) :: ((Int, Int), Map.Map (Int, Int) [(Int, Int)])) =
  walk (Set.fromList [n0]) 2 n1
  where
    neighbors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    neighborsPipes =
      Maybe.mapMaybe (\n -> fmap (n, ) $ n `Map.lookup` map) neighbors
    n0 = fst $ head $ filter (\(n, cs) -> s `elem` cs) neighborsPipes
    n1 = head $ filter (s /=) $ map Map.! n0
    walk visited count current
      | current == s = (Set.insert s visited, count)
      | current `Set.member` visited = (visited, count)
      | otherwise =
        let unvisited = filter (`Set.notMember` visited) $ map Map.! current
         in if null unvisited
              then (visited, count)
              else walk
                     (Set.insert current visited)
                     (count + 1)
                     (head unvisited)

parseInput2 input =
  let (Just s, map) =
        foldl
          parseLine
          (Nothing, Map.empty :: (Map.Map (Int, Int) Char))
          linesWithY
   in (s, map, length $ snd $ head linesWithY, length linesWithY)
  where
    linesWithY = zip [0 ..] $ lines input
    parseLine (acc :: (Maybe (Int, Int), Map.Map (Int, Int) Char)) (y :: Int, line) =
      foldl (parseCell y) acc $ zip [0 ..] line
    parseCell (y :: Int) (s, map :: (Map.Map (Int, Int) Char)) (x :: Int, c) =
      let s' =
            if c == 'S'
              then Just (x, y)
              else s
          map' = Map.insert (x, y) c map
       in (s', map')

newtype OrdSet a =
  OrdSet [a]
  deriving (Show)

osInsert :: Ord a => a -> OrdSet a -> OrdSet a
osInsert a (OrdSet l) = OrdSet $ ins a l
  where
    ins a [] = [a]
    ins a (x:xs)
      | a == x = x : xs
      | a <= x = a : x : xs
      | otherwise = x : ins a xs

osMember :: Eq a => a -> OrdSet a -> Bool
osMember a (OrdSet l) = a `elem` l

osNotMember :: Eq a => a -> OrdSet a -> Bool
osNotMember a (OrdSet l) = a `notElem` l

rights = "L-F"

lefts = "7-J"

downs = "7|F"

ups = "J|L"

floodFill (sx, sy) width height map0 path =
  length (fill (OrdSet [(sx - 1, sy - 1)]) Set.empty Set.empty)
  where
    [u, l, r, d] =
      fmap
        (`Map.lookup` map0)
        [(sx, sy - 1) :: (Int, Int), (sx - 1, sy), (sx + 1, sy), (sx, sy + 1)]
    t =
      ( maybe False (`elem` downs) u
      , maybe False (`elem` rights) l
      , maybe False (`elem` lefts) r
      , maybe False (`elem` ups) d)
    s =
      case t of
        (_, True, True, _) -> '-'
        (True, _, _, True) -> '|'
        (_, True, _, True) -> '7'
        (True, True, _, _) -> 'J'
        (True, _, True, _) -> 'L'
        (_, _, True, True) -> 'F'
    map = Map.insert (sx, sy) s map0
    spacesInMap = length $ filter (== '.') $ Map.elems map
    isConnected a@(ax, ay) b@(bx, by) =
      case (Map.lookup a map, Map.lookup b map) of
        (Just pa, Just pb) ->
          if ay == by
            then pa `elem` rights && pb `elem` lefts
            else pa `elem` downs && pb `elem` ups
        _ -> False
    insideExtendedBounds (x, y) = x >= 0 && y >= 0 && x <= width && y <= height
    isNotOfPath n = n `Set.notMember` path
    fill (OrdSet []) visited spaces = spaces
    fill (OrdSet (u@(x, y):us)) visited spaces =
      let visited' = Set.insert u visited
          unvisitedNeighbors =
            fst <$>
            filter
              (\(n@(x, y), p) ->
                 insideExtendedBounds n && p && n `Set.notMember` visited)
              [ ((x + 1, y), not (isConnected (x, y - 1) (x, y)))
              , ((x, y + 1), not (isConnected (x - 1, y) (x, y)))
              , ((x - 1, y), not (isConnected (x - 1, y - 1) (x - 1, y)))
              , ((x, y - 1), not (isConnected (x - 1, y - 1) (x, y - 1)))
              ]
          neighborSpaces =
            filter isNotOfPath [(x - 1, y - 1), (x, y - 1), (x - 1, y), (x, y)]
          spaces' = foldl (flip Set.insert) spaces neighborSpaces
          unvisited' = foldl (flip osInsert) (OrdSet us) unvisitedNeighbors
       in fill unvisited' visited' spaces'

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  let parsed = parseInput input
  let (path, pathLength) = findPath parsed
  print $ pathLength `div` 2
  putStrLn "Part 2"
  let (s, map, w, h) = parseInput2 input
  let result = floodFill s w h map path
  print result
