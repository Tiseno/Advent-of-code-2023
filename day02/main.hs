{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
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
          fmap (fmap parseCube . Split.splitOn ", ") $ Split.splitOn "; " line'
    parseCube s =
      let (d, s') = Bifunctor.first read $ span Char.isDigit s
       in (case drop 1 s' of
             "red"   -> Red
             "green" -> Green
             "blue"  -> Blue)
            d

part1 (redLimit, greenLimit, blueLimit) (input :: [Game]) =
  sum $ fmap gameId $ filter possibleGame input
  where
    possibleGame Game {gameRounds} = all cubePossible $ concat gameRounds
    cubePossible (Red i)   = i <= redLimit
    cubePossible (Green i) = i <= greenLimit
    cubePossible (Blue i)  = i <= blueLimit

part2 (input :: [Game]) = sum $ fmap (power . findMax) input
  where
    power (r, g, b) = r * g * b
    findMax Game {gameRounds} = foldl maxFold (0, 0, 0) $ concat gameRounds
    maxFold (r, g, b) (Red i)   = (max r i, g, b)
    maxFold (r, g, b) (Green i) = (r, max g i, b)
    maxFold (r, g, b) (Blue i)  = (r, g, max b i)

main = do
  input <- readFile "input.txt"
  let games = parseInput input
  putStrLn "Part 1"
  print $ part1 (12, 13, 14) games
  putStrLn "Part 2"
  print $ part2 games
