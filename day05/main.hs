import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Debug.Trace     as Debug

parseInput :: String -> ([Int], [(String, [(Int, Int, Int)])])
parseInput input =
  let seedLine:mapLines = lines input
      seeds = fmap read $ words $ dropWhile (/= ' ') seedLine
      maps = fmap parseMap $ drop 1 $ Split.splitOn [""] mapLines
   in (seeds, maps)
  where
    parseMap (name:ranges) = (name, fmap parseRange ranges)
    parseRange rangeLine =
      let [dst, src, range] = words rangeLine
       in (read dst :: Int, read src :: Int, read range :: Int)

part1 :: ([Int], [(String, [(Int, Int, Int)])]) -> Int
part1 input = minimum $ fmap findSeedLocation seeds
  where
    (seeds, mappings) = input
    findSeedLocation seed = foldl f seed mappings
    f seed (name, singleMappings) =
      case List.find
             (\(_, src, range) -> seed >= src && seed <= src + range)
             singleMappings of
        Just (dst, src, range) -> dst + (seed - src)
        Nothing                -> seed

concatTuples :: [([(Int, Int)], [(Int, Int)])] -> ([(Int, Int)], [(Int, Int)])
concatTuples = foldl (\(a, b) (a', b') -> (a ++ a', b ++ b')) ([], [])

part2 (input :: ([Int], [(String, [(Int, Int, Int)])])) =
  minimum $ fst <$> concatMap fst findAllLocationRanges
  where
    (seeds, mappings) = input
    seedRanges []         = []
    seedRanges (s:len:xs) = (s, len) : seedRanges xs
    findAllLocationRanges = fmap findLocationsForRange (seedRanges seeds)
    findLocationsForRange seedRange =
      foldl findRangesFromOneMapping ([seedRange], [] :: [(Int, Int)]) mappings
    findRangesFromOneMapping acc (name, rangeMappings) =
      let (left, produced) =
            foldl (findRangesFromOneRangeMapping name) acc rangeMappings
       in (left ++ produced, [])
    findRangesFromOneRangeMapping name (left, produced) rangeMapping =
      let (left', produced') =
            concatTuples $ fmap (projectAndSplit rangeMapping) left
       in (left', produced' ++ produced)
    projectAndSplit (dst, src, len) (a, l) -- on left or right of the range
      | (a + l) < src || a > src + len = ([(a, l)], [])
    projectAndSplit (dst, src, len) (a, l) -- enclosing mapping fully
      | a < src && a + l > src + len =
        ([(a, src - a), (src + len, a + l - (src + len))], [(dst, l)])
    projectAndSplit (dst, src, len) (a, l) -- fully enclosed by mapping
      | a >= src && (a + l) <= src + len = ([], [(dst + (a - src), l)])
    projectAndSplit (dst, src, len) (a, l) -- left inside but right outside
      | a >= src && (a + l) > src + len =
        ([(src + len, a + l - (src + len))], [(dst + (a - src), src + len - a)])
    projectAndSplit (dst, src, len) (a, l) -- left outside but right inside
     = ([(a, src - a)], [(dst, a + l - src)])

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
  putStrLn "Part 2"
  print $ part2 parsed
