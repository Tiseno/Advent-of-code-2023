import qualified Data.List.Split as Split
import qualified Data.Map        as Map

newtype Node =
  Node (Char, Char, Char)
  deriving (Eq, Ord)

lastChar (Node (_, _, c)) = c

instance Show Node where
  show (Node (a, b, c)) = [a, b, c]

instance Read Node where
  readsPrec _ (a:b:c:xs) = [(Node (a, b, c), xs)]
  readsPrec _ _          = []

parseInput :: String -> ([Char], [(Node, (Node, Node))])
parseInput input =
  let [steps, l] = Split.splitOn "\n\n" input
   in (steps, parseLine <$> lines l)
  where
    parseLine l =
      let [src, destinations] = Split.splitOn " = " l
       in (read src, read $ filter (/= ' ') destinations)

choose 'L' (l, _) = l
choose 'R' (_, r) = r

part1 (steps, nodes) = walk (cycle steps) 0 startingNode
  where
    nodeMap = Map.fromList nodes
    startingNode = read "AAA"
    targetNode = read "ZZZ"
    walk _ count current
      | current == targetNode = count
    walk (x:steps) count current =
      walk steps (count + 1) (choose x $ nodeMap Map.! current)

part2 (steps, nodes) = walk (cycle steps) 0 startingNodes
  where
    nodeMap = Map.fromList nodes
    startingNodes = filter (('A' ==) . lastChar) $ fmap fst nodes
    targetChar = 'Z'
    walk _ count currents
      | all ((targetChar ==) . lastChar) currents = count
    walk (x:steps) count currents =
      walk steps (count + 1) $ choose x <$> fmap (nodeMap Map.!) currents

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
  putStrLn "Part 2"
  print $ part2 parsed
