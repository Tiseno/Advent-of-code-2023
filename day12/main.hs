import qualified Data.Bifunctor  as Bifunctor
import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Map        as Map

parseRecords input = parseRecord <$> lines input
  where
    parseRecord line =
      Bifunctor.second (\d -> read <$> Split.splitOn "," d :: [Int]) $
      (\[layout, groups] -> (layout, groups)) $ Split.splitOn " " line

arrangementSum (input :: [(String, [Int])]) = sum $ fst <$> fmap solveLine input

solveLine ((layout, groups) :: (String, [Int])) = search Map.empty layout groups

canBeDamaged c = c == '?' || c == '#'

canBeOperational c = c == '?' || c == '.'

type Memo = Map.Map (Int, Int) Int

search :: Memo -> String -> [Int] -> (Int, Memo)
search m layout []
  | all canBeOperational layout = (1, m)
search m _ [] = (0, m)
search m [] (_:_) = (0, m)
search m layout@(l:ls) groups@(g:gs) =
  let key = (length layout, length groups)
   in case Map.lookup key m of
        Just i -> (i, m)
        Nothing ->
          let nextPossibleDamaged = take g layout
              afterPossibleDamaged = drop g $ take (g + 1) layout
              (r1, m') =
                if length nextPossibleDamaged == g &&
                   all canBeDamaged nextPossibleDamaged &&
                   all canBeOperational afterPossibleDamaged
                  then search m (drop (g + 1) layout) gs
                  else (0, m)
              (r2, m'') =
                if canBeOperational l
                  then search m' ls groups
                  else (0, m')
              r = r1 + r2
           in (r, Map.insert key r m'')

unfoldRecords (layout, groups) =
  (List.intercalate "?" $ replicate 5 layout, concat $ replicate 5 groups)

main = do
  input <- readFile "input.txt"
  let records = parseRecords input
  putStrLn "Part 1"
  print $ arrangementSum records
  putStrLn "\nPart 2"
  print $ arrangementSum $ fmap unfoldRecords records
