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

part2 input = input

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
