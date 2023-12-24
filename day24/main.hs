import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Maybe      as Maybe
import qualified Data.Ratio      as Rational
import qualified Debug.Trace     as Debug

type Pos = (Rational, Rational, Rational)

type Vel = (Rational, Rational, Rational)

type Stone = (Pos, Vel)

parseInput input = parseLine <$> lines input
  where
    parseLine l =
      let [pos, vel] = parseGroup <$> Split.splitOn " @ " l
       in (pos, vel) :: Stone
    parseGroup g =
      let [a, b, c] = read <$> Split.splitOn ", " g
       in (a Rational.% 1, b Rational.% 1, c Rational.% 1)

futureIntersection :: Stone -> Stone -> Maybe Pos
futureIntersection ((aX, aY, _), (aDX, aDY, _)) ((bX, bY, _), (bDX, bDY, _))
  | aDX * bDY == aDY * bDX = Nothing
futureIntersection ((aX, aY, _), (aDX, aDY, _)) ((bX, bY, _), (bDX, bDY, _)) =
  if (aDX == 0 && x == aX || aDX > 0 && x >= aX || aDX < 0 && x <= aX) &&
     (aDY == 0 && y == aY || aDY > 0 && y >= aY || aDY < 0 && y <= aY) &&
     (bDX == 0 && x == bX || bDX > 0 && x >= bX || bDX < 0 && x <= bX) &&
     (bDY == 0 && y == bY || bDY > 0 && y >= bY || bDY < 0 && y <= bY)
    then Just (x, y, 0)
    else Nothing
  where
    am = aY - aX * aDY / aDX
    ak = aDY / aDX
    bm = bY - bX * bDY / bDX
    bk = bDY / bDX
    x = (bm - am) / (ak - bk)
    y = ak * x + am

part1 (stones, areaMin :: Rational, areaMax :: Rational) =
  length $ filter isWithinTestArea allFutureIntersections
  where
    isWithinTestArea (x, y, _) =
      x >= areaMin && x <= areaMax && y >= areaMin && y <= areaMax
    allFutureIntersections =
      Maybe.mapMaybe (uncurry futureIntersection) allPairs
    pairFirstWithRest (x:xs) = (x, ) <$> xs
    pairFirstWithRest _      = []
    allPairs = concatMap pairFirstWithRest $ List.tails stones

main = do
  exampleSetup <- (, 7, 27) . parseInput <$> readFile "input.example.txt"
  putStrLn "Part 1 - example"
  print $ part1 exampleSetup
  setup <-
    (, 200000000000000, 400000000000000) . parseInput <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 setup
