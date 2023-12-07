import qualified Data.List     as List
import qualified Data.Map      as Map
import qualified Data.MultiSet as MultiSet
import qualified Debug.Trace   as Debug

data Hand
  = NormalHand
      { cards :: String
      , bid   :: Int
      }
  | JokerHand
      { cards :: String
      , bid   :: Int
      }
  deriving (Eq)

instance Show Hand where
  show hand =
    cards hand ++ " " ++ show (bid hand) ++ " " ++ show (toHandType hand)

parseInput input h = parseLine <$> lines input
  where
    parseLine line =
      let [cards, bid] = words line
       in h cards (read bid)

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
shapeToHandType [2, 1, 1, 1]    = OnePair
shapeToHandType [2, 2, 1]       = TwoPair
shapeToHandType [3, 1, 1]       = Threes
shapeToHandType [3, 2]          = FullHouse
shapeToHandType [4, 1]          = Fours
shapeToHandType [5]             = Fives

toHandType NormalHand {cards} =
  shapeToHandType $
  List.reverse $
  List.sort $ fmap snd $ MultiSet.toOccurList $ MultiSet.fromList cards
toHandType JokerHand {cards} =
  let cardSet = MultiSet.fromList cards
      jokers = MultiSet.occur 'J' cardSet
      normalCards = MultiSet.deleteAll 'J' cardSet
      shape =
        List.reverse $ List.sort $ snd <$> MultiSet.toOccurList normalCards
      shape' =
        case shape of
          []     -> [jokers]
          (x:xs) -> (x + jokers) : xs
   in shapeToHandType shape'

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

jokerCardValues = Map.insert 'J' 1 $ Map.delete 'J' cardValues

cardValuesForHandTypes NormalHand {} NormalHand {} = cardValues
cardValuesForHandTypes JokerHand {} JokerHand {}   = jokerCardValues
cardValuesForHandTypes _ _                         = undefined

compareFirstDifferingCard cardValues a b =
  uncurry (<=) $
  head $
  filter (uncurry (/=)) $
  zip (fmap (cardValues Map.!) a) (fmap (cardValues Map.!) b)

instance Ord Hand where
  handA <= handB =
    let handTypeA = toHandType handA
        handTypeB = toHandType handB
     in if handTypeA == handTypeB
          then compareFirstDifferingCard
                 (cardValuesForHandTypes handA handB)
                 (cards handA)
                 (cards handB)
          else handTypeA <= handTypeB

solve hands = sum $ fmap (uncurry (*)) <$> zip [1 ..] $ bid <$> List.sort hands

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  print $ solve $ parseInput input NormalHand
  putStrLn "Part 2"
  print $ solve $ parseInput input JokerHand
