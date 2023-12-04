{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
import qualified Data.List.Split as Split
import qualified Data.MultiSet   as MultiSet
import qualified Data.Set        as Set

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

part2 input =
  sum $
  fmap snd $
  MultiSet.toOccurList $
  foldl scoreOfCard (MultiSet.fromList [1 .. (length input)]) input
  where
    scoreOfCard cardCount Card {cardId, winningNumbers, ourNumbers} =
      let wins = length $ winningNumbers `Set.intersection` ourNumbers
          cardIds = [(cardId + 1) .. (cardId + wins)]
       in countWins (MultiSet.occur cardId cardCount) cardCount cardIds
    countWins multiplier cardCount [] = cardCount
    countWins multiplier cardCount (x:xs) =
      countWins multiplier (MultiSet.insertMany x multiplier cardCount) xs

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
  putStrLn "Part 2"
  print $ part2 parsed
