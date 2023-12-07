import qualified Data.List        as List
import qualified Data.Map         as Map
import qualified Data.MultiSet    as MultiSet
import qualified Data.Tuple.Extra as TupleExtra

data Hand =
  Hand
    { cards :: String
    , bid   :: Int
    }
  deriving (Eq, Show)

parseInput input = parseLine <$> lines input
  where
    parseLine line =
      let [cards, bid] = words line
       in Hand cards (read bid)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | Threes
  | FullHouse
  | Fours
  | Fives
  deriving (Eq, Ord, Show)

shapeToHandType :: [Int] -> HandType
shapeToHandType [1, 1, 1, 1, 1] = HighCard
shapeToHandType [2, 1, 1, 1]    = OnePair
shapeToHandType [2, 2, 1]       = TwoPair
shapeToHandType [3, 1, 1]       = Threes
shapeToHandType [3, 2]          = FullHouse
shapeToHandType [4, 1]          = Fours
shapeToHandType [5]             = Fives

toHandTypeFromNormalCards cards =
  shapeToHandType $
  List.reverse $
  List.sort $ fmap snd $ MultiSet.toOccurList $ MultiSet.fromList cards

normalCardValues =
  Map.fromList $
  zip ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'] [2 ..]

firstDifferingCards a b = head $ filter (uncurry (/=)) $ zip a b

handCompare (toHandType, cardValues) Hand {cards = cardsA} Hand {cards = cardsB} =
  let handTypeA = toHandType cardsA
      handTypeB = toHandType cardsB
   in if handTypeA == handTypeB
        then uncurry compare $
             TupleExtra.both (cardValues Map.!) $
             firstDifferingCards cardsA cardsB
        else compare handTypeA handTypeB

toHandTypeFromJokerCards cards =
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

jokerCardValues = Map.insert 'J' 1 $ Map.delete 'J' normalCardValues

normalRules = (toHandTypeFromNormalCards, normalCardValues)

jokerRules = (toHandTypeFromJokerCards, jokerCardValues)

solve rules hands =
  sum $
  fmap (uncurry (*)) $
  zip [1 ..] $ bid <$> List.sortBy (handCompare rules) hands

main = do
  input <- readFile "input.txt"
  let hands = parseInput input
  putStrLn "Part 1"
  print $ solve normalRules hands
  putStrLn "Part 2"
  print $ solve jokerRules hands
