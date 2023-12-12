import qualified Data.Bifunctor  as Bifunctor
import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Map        as Map

parseRecords input = parseRecord <$> lines input
  where
    parseRecord line =
      Bifunctor.second (\d -> read <$> Split.splitOn "," d :: [Int]) $
      (\[layout, groups] -> (layout, groups)) $ Split.splitOn " " line

arrangementSum (input :: [(String, [Int])]) = sum $ fmap arrangements input

type Memo = Map.Map (Int, Int) Int

arrangements :: (String, [Int]) -> Int
arrangements (layout, group) = snd $ search' Map.empty layout group
  where
    canBeDamaged c = c == '?' || c == '#'
    canBeOperational c = c == '?' || c == '.'
    search' :: Memo -> String -> [Int] -> (Memo, Int)
    search' m layout []
      | all canBeOperational layout = (m, 1)
    search' m _ [] = (m, 0)
    search' m [] (_:_) = (m, 0)
    search' m layout@(l:ls) groups@(g:gs) =
      let key = (length layout, length groups)
       in case Map.lookup key m of
            Just i -> (m, i)
            Nothing ->
              let nextPossibleDamaged = take g layout
                  afterPossibleDamaged = drop g $ take (g + 1) layout
                  (m', r1) =
                    if length nextPossibleDamaged == g &&
                       all canBeDamaged nextPossibleDamaged &&
                       all canBeOperational afterPossibleDamaged
                      then search' m (drop (g + 1) layout) gs
                      else (m, 0)
                  (m'', r2) =
                    if canBeOperational l
                      then search' m' ls groups
                      else (m', 0)
                  r = r1 + r2
               in (Map.insert key r m'', r)

unfoldRecords (layout, groups) =
  (List.intercalate "?" $ replicate 5 layout, concat $ replicate 5 groups)

main = do
  input <- readFile "input.txt"
  let records = parseRecords input
  putStrLn "Part 1"
  print $ arrangementSum records
  putStrLn "\nPart 2"
  print $ arrangementSum $ fmap unfoldRecords records
