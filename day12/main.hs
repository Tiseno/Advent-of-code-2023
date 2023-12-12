import qualified Data.Bifunctor  as Bifunctor
import qualified Data.List.Split as Split

parseInput input = parseLine <$> lines input
  where
    parseLine line =
      Bifunctor.second (\d -> read <$> Split.splitOn "," d :: [Int]) $
      (\[layout, groups] -> (layout, groups)) $ Split.splitOn " " line

part1 (input :: [(String, [Int])]) = sum $ length <$> fmap solveLine input

solveLine ((layout, groups) :: (String, [Int])) = search "" layout groups

canBeDamaged c = c == '?' || c == '#'

canBeOperational c = c == '?' || c == '.'

isUnknown c = c == '?'

search :: String -> String -> [Int] -> [String]
search prev layout [] =
  [prev ++ replicate (length layout) '.' | all canBeOperational layout]
search prev [] (_:_) = []
search prev layout@(l:ls) groups@(g:gs) =
  let nextPossibleDamaged = take g layout
      afterPossibleDamaged = drop g $ take (g + 1) layout
   in (if length nextPossibleDamaged == g &&
          all canBeDamaged nextPossibleDamaged &&
          all canBeOperational afterPossibleDamaged
         then search
                (prev ++
                 ('#' <$ nextPossibleDamaged) ++ ('.' <$ afterPossibleDamaged))
                (drop (g + 1) layout)
                gs
         else []) ++
      if canBeOperational l
        then search (prev ++ ".") ls groups
        else []

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
