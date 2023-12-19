import qualified Data.Char       as Char
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import qualified Data.Maybe      as Maybe

type Name = String

data Rule =
  Rule (Maybe (Char, Char, Int)) Name
  deriving (Show, Eq)

type Workflow = (Name, [Rule])

data Part a =
  Part
    { _x :: a
    , _m :: a
    , _a :: a
    , _s :: a
    }
  deriving (Show, Eq)

parseInput input =
  let [workflows, parts] = lines <$> Split.splitOn "\n\n" input
   in (parseWorkflow <$> workflows :: [Workflow], parsePart <$> parts)
  where
    parseWorkflow wf =
      let (name, rest) = span Char.isAlpha wf
          r = init $ tail rest
       in (name, readRule <$> Split.splitOn "," r)
    parsePart p =
      let [_:_:xs, _:_:ms, _:_:as, _:_:ss] = Split.splitOn "," $ init $ tail p
       in Part (read xs) (read ms) (read as) (read ss)
    readRule s =
      let (targetOrCat, rest) = span Char.isAlpha s
       in if null rest
            then Rule Nothing targetOrCat
            else let [rule, target] = Split.splitOn ":" rest
                     (op:i) = rule
                  in Rule (Just (head targetOrCat, op, read i)) target

part1 (workflows :: [Workflow]) (parts :: [Part Int]) =
  foldl (\a (Part x1 m1 a1 s1) -> a + x1 + m1 + a1 + s1) 0 acceptedParts
  where
    acceptedParts = filter (runWorkflows "in") parts
    workflowMap = Map.fromList workflows
    runWorkflows name part =
      let rules = workflowMap Map.! name
          ms = Maybe.mapMaybe (runRule part) rules
          result = head ms
       in case result of
            ['A']   -> True
            ['R']   -> False
            newName -> runWorkflows newName part
    runRule _ (Rule Nothing target) = Just target
    runRule part (Rule (Just (cat, op, i)) target) =
      let a =
            case cat of
              'x' -> _x part
              'm' -> _m part
              'a' -> _a part
              's' -> _s part
          fn =
            case op of
              '<' -> (<)
              '>' -> (>)
       in if fn a i
            then Just target
            else Nothing

part2 workflows =
  sum $
  (\(_, p) -> partCombinations p) <$>
  filter
    (\(t, p) -> t == "A")
    ((!! 7) $
     iterate runWorkflows [("in", Part (1, 4000) (1, 4000) (1, 4000) (1, 4000))])
  where
    partCombinations (Part (x, x') (m, m') (a, a') (s, s')) =
      (1 + x' - x) * (1 + m' - m) * (1 + a' - a) * (1 + s' - s)
    runWorkflows (targetAndPart :: [(Name, Part (Int, Int))]) =
      concatMap fst (runWorkflow <$> targetAndPart)
    runWorkflow (['A'], part) = ([("A", part)], Nothing)
    runWorkflow (['R'], part) = ([("R", part)], Nothing)
    runWorkflow (target, part) =
      foldl foldRule ([], Just part) (workflowMap Map.! target)
    workflowMap = Map.fromList workflows
    foldRule (acc :: [(Name, Part (Int, Int))], Nothing) _ = (acc, Nothing)
    foldRule (acc, Just part :: Maybe (Part (Int, Int))) (Rule Nothing target) =
      (acc ++ [(target, part)], Nothing)
    foldRule (acc, Just part :: Maybe (Part (Int, Int))) (Rule (Just (cat, op, i)) target) =
      let (matching, notMatching) = slicePartOnCategory part cat op i
          acc' =
            case matching of
              Just m  -> acc ++ [(target, m)]
              Nothing -> acc
       in (acc', notMatching)
    sliceRange (min, max) i '<'
      | max < i = (Just (min, max), Nothing)
      | i <= min = (Nothing, Just (min, max))
      | otherwise = (Just (min, i - 1), Just (i, max))
    sliceRange (min, max) i '>'
      | min > i = (Just (min, max), Nothing)
      | i >= max = (Nothing, Just (min, max))
      | otherwise = (Just (i + 1, max), Just (min, i))
    slicePartOnCategory (part :: Part (Int, Int)) catName op i =
      let (cat, catUpdate) =
            case catName of
              'x' -> (_x, \p v -> p {_x = v})
              'm' -> (_m, \p v -> p {_m = v})
              'a' -> (_a, \p v -> p {_a = v})
              's' -> (_s, \p v -> p {_s = v})
          range = cat part
          (matchedRange, notMatchedRange) = sliceRange range i op
          m =
            ( catUpdate part <$> matchedRange
            , catUpdate part <$> notMatchedRange)
       in m

main = do
  input <- readFile "input.txt"
  let (workflows, parts) = parseInput input
  putStrLn "Part 1"
  print $ part1 workflows parts
  putStrLn "Part 2"
  print $ part2 workflows
