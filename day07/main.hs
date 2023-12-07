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

toHandShapeFromNormalCards cards =
  List.reverse $
  List.sort $ fmap snd $ MultiSet.toOccurList $ MultiSet.fromList cards

normalCardValues =
  Map.fromList $
  zip ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'] [2 ..]

firstDiffering a b = head $ filter (uncurry (/=)) $ zip a b

handCompare (toHandShape, cardValues) Hand {cards = cardsA} Hand {cards = cardsB} =
  let handShapes = (toHandShape cardsA, toHandShape cardsB)
      cardsOrShapes =
        if uncurry (==) handShapes
          then TupleExtra.both (fmap (cardValues Map.!)) (cardsA, cardsB)
          else handShapes
   in uncurry compare $ uncurry firstDiffering cardsOrShapes

toHandShapeFromJokerCards cards =
  let cardSet = MultiSet.fromList cards
      jokers = MultiSet.occur 'J' cardSet
      normalCards = MultiSet.deleteAll 'J' cardSet
   in case List.reverse $ List.sort $ snd <$> MultiSet.toOccurList normalCards of
        []     -> [jokers]
        (x:xs) -> (x + jokers) : xs

jokerCardValues = Map.insert 'J' 1 $ Map.delete 'J' normalCardValues

normalRules = (toHandShapeFromNormalCards, normalCardValues)

jokerRules = (toHandShapeFromJokerCards, jokerCardValues)

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
