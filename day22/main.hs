import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import qualified Data.Ord        as Ord

newtype Id =
  Id Int
  deriving (Show, Eq, Ord)

data Pos =
  Pos
    { posX :: Int
    , posY :: Int
    , posZ :: Int
    }
  deriving (Show, Eq, Ord)

data Size =
  Size
    { sizeW :: Int
    , sizeL :: Int
    , sizeH :: Int
    }
  deriving (Show, Eq, Ord)

data Block =
  Block
    { bId  :: String
    , size :: Size
    }
  deriving (Show, Eq, Ord)

toId i =
  let (q, r) = i `quotRem` length letters
   in (if q == 0
         then []
         else toId q) ++
      [letters !! r]
  where
    letters = ['A' .. 'Z']

parseBlocks input = snd $ blocks $ lines input
  where
    blocks = foldl (\(i, p) l -> (succ i, parseBlock i l : p)) (0, [])
    parseBlock (i :: Int) l =
      let [start@(x0, y0, z0), end@(x1, y1, z1)] =
            parsePosition <$> Split.splitOn "~" l
          (x, y, z) = (min x0 x1, min y0 y1, min z0 z1)
          (width, length, height) =
            (abs (x0 - x1) + 1, abs (y0 - y1) + 1, abs (z0 - z1) + 1)
       in (Pos x y z, Block (toId i) (Size width length height))
    parsePosition b =
      let [x, y, z] :: [Int] = read <$> Split.splitOn "," b
       in (x, y, z)

-- (supports, supportedBy)
type Graph a = Map.Map a ([a], [a])

settleBlocksIntoGraph (blocks :: [(Pos, Block)]) =
  foldl settleBlock (0, [], Map.empty) sortedByZ
  where
    sortedByZ = List.sortBy (Ord.comparing (posZ . fst)) blocks
    zTopOf (p, b) = posZ p + sizeH (size b)
    xyOverlaps i a@(aP, aB) b@(bP, bB) =
      let aLeft = posX aP
          aRight = posX aP + sizeW (size aB) - 1
          aBot = posY aP
          aTop = posY aP + sizeL (size aB) - 1
          bLeft = posX bP
          bRight = posX bP + sizeW (size bB) - 1
          bBot = posY bP
          bTop = posY bP + sizeL (size bB) - 1
       in not (aRight < bLeft || bRight < aLeft || aTop < bBot || bTop < aBot)
    higherThan settled posBlock =
      let top = zTopOf posBlock
       in all (\pb -> top >= zTopOf pb) settled
    supportsUpdate (a :: String) (graph :: Graph String) (b :: String) =
      let (supports, supportedBy) = graph Map.! b
       in Map.insert b (a : supports, supportedBy) graph
    settleBlock (count, settled, graph) (pos, block) =
      let settledOverlapping = filter (xyOverlaps count (pos, block)) settled
          highestOverlapping =
            filter (higherThan settledOverlapping) settledOverlapping
          newZ =
            if null highestOverlapping
              then 0
              else zTopOf $ head highestOverlapping
          newPos = pos {posZ = newZ}
          supportedBy = bId . snd <$> highestOverlapping
          graph' = foldl (supportsUpdate (bId block)) graph supportedBy
       in ( count + 1
          , settled ++ [(newPos, block)]
          , Map.insert (bId block) ([], supportedBy) graph')

part1 (graph :: Graph String) =
  length $ filter id $ canBeDisintegrated <$> Map.keys graph
  where
    supportedByMultiple n = length (snd $ graph Map.! n) > 1
    canBeDisintegrated n = all supportedByMultiple $ fst $ graph Map.! n

main = do
  input <- readFile "input.txt"
  let parsed = parseBlocks input
  putStrLn "Part 1"
  let (_, _, g) = settleBlocksIntoGraph parsed
  print $ part1 g
