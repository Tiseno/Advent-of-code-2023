{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant id" #-}
import qualified Data.Bifunctor  as Bifunctor
import qualified Data.Char       as Char
import qualified Data.List.Split as Split

data Game =
  Game
    { gameId     :: Int
    , gameRounds :: [[Cube]]
    }
  deriving (Show)

data Cube
  = Red Int
  | Green Int
  | Blue Int
  deriving (Show)

parseInput input = fmap parseLine $ lines input
  where
    parseLine line =
      let [gs, line'] = Split.splitOn ": " line
          i = read $ drop 5 gs
       in Game i $
          fmap (fmap parseCube) $
          fmap (Split.splitOn ", ") $ Split.splitOn "; " line'
    parseCube s =
      let (d, s') = Bifunctor.first read $ span Char.isDigit s
       in (case drop 1 s' of
             "red"   -> Red
             "green" -> Green
             "blue"  -> Blue)
            d

part1 (redLimit, greenLimit, blueLimit) (input :: [Game]) =
  sum $ fmap gameId $ filter f input
  where
    f Game {gameRounds} = all (all cubePossible) gameRounds
    cubePossible (Red i)   = i <= redLimit
    cubePossible (Green i) = i <= greenLimit
    cubePossible (Blue i)  = i <= blueLimit

main = do
  input <- readFile "input.txt"
  let games = parseInput input
  putStrLn "Part 1"
  print $ part1 (12, 13, 14) games
