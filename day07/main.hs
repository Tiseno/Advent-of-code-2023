import qualified Data.List     as List
import qualified Data.Map      as Map
import qualified Data.MultiSet as MultiSet

data Hand =
  Hand
    { cards :: String
    , bid   :: Int
    }
  deriving (Eq)

instance Show Hand where
  show Hand {cards, bid} =
    cards ++ " " ++ show bid ++ " " ++ show (toHandType cards)

parseInput input = parseLine <$> lines input
  where
    parseLine line =
      let [cards, bid] = words line
       in Hand {cards = cards, bid = read bid}

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | Threes
  | FullHouse
  | Fours
  | Fives
  deriving (Enum, Eq, Ord, Show)

shapeToHandType :: [Int] -> HandType
shapeToHandType [1, 1, 1, 1, 1] = HighCard
shapeToHandType [1, 1, 1, 2]    = OnePair
shapeToHandType [1, 2, 2]       = TwoPair
shapeToHandType [1, 1, 3]       = Threes
shapeToHandType [2, 3]          = FullHouse
shapeToHandType [1, 4]          = Fours
shapeToHandType [5]             = Fives

toHandType cards =
  shapeToHandType $
  List.sort $ fmap snd $ MultiSet.toOccurList $ MultiSet.fromList cards

cardValues =
  Map.fromList
    [ ('2', 2)
    , ('3', 3)
    , ('4', 4)
    , ('5', 5)
    , ('6', 6)
    , ('7', 7)
    , ('8', 8)
    , ('9', 9)
    , ('T', 10)
    , ('J', 11)
    , ('Q', 12)
    , ('K', 13)
    , ('A', 14)
    ]

compareFirstDifferingCard (a:cardsA) (b:cardsB)
  | a == b = compareFirstDifferingCard cardsA cardsB
compareFirstDifferingCard (a:cardsA) (b:cardsB) =
  cardValues Map.! a <= cardValues Map.! b

instance Ord Hand where
  Hand {cards = cardsA} <= Hand {cards = cardsB} =
    let handTypeA = toHandType cardsA
        handTypeB = toHandType cardsB
     in if handTypeA == handTypeB
          then compareFirstDifferingCard cardsA cardsB
          else handTypeA <= handTypeB

part1 input = sum $ fmap (uncurry (*)) <$> zip [1 ..] $ bid <$> List.sort input

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
