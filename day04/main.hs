import qualified Data.List.Split as Split
import qualified Data.Set        as Set
import qualified Debug.Trace     as Debug

data Card =
  Card
    { cardId         :: Int
    , winningNumbers :: Set.Set Int
    , ourNumbers     :: Set.Set Int
    }
  deriving (Show)

parseInput input = fmap parseLine $ lines input
  where
    parseLine line =
      let [ci, line'] = Split.splitOn ": " line
          [winning, our] = fmap words $ Split.splitOn "|" line'
       in Card
            (read $ drop 5 ci)
            (Set.fromList $ fmap read winning)
            (Set.fromList $ fmap read our)

part1 input = sum $ fmap scoreOfCard input
  where
    scoreOfCard Card {winningNumbers, ourNumbers} =
      let n = length $ winningNumbers `Set.intersection` ourNumbers
       in if n == 0
            then 0
            else 2 ^ (n - 1)

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
