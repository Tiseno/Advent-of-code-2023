import qualified Data.List.Split as Split
import qualified Data.Map        as Map

newtype Node =
  Node (Char, Char, Char)
  deriving (Eq, Ord)

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

part1 (steps, nodes) = walk 0 (cycle steps) (read "AAA") (read "ZZZ")
  where
    nodeMap = Map.fromList nodes
    walk count _ current target
      | current == target = count
    walk count (x:steps) current target =
      let (l, r) = nodeMap Map.! current
       in case x of
            'L' -> walk (count + 1) steps l target
            'R' -> walk (count + 1) steps r target

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
