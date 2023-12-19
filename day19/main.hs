import qualified Data.Char       as Char
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import qualified Data.Maybe      as Maybe

type Name = String

data Rule =
  Rule (Maybe (Char, Char, Int)) Name
  deriving (Show, Eq)

type Workflow = (Name, [Rule])

data Part =
  Part
    { _x :: Int
    , _m :: Int
    , _a :: Int
    , _s :: Int
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

part1 (workflows :: [Workflow]) (parts :: [Part]) =
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

main = do
  input <- readFile "input.txt"
  let (workflows, parts) = parseInput input
  putStrLn "Part 1"
  print $ part1 workflows parts
