import qualified Data.List       as List
import qualified Data.List.Split as Split
import qualified Data.Map        as Map
import qualified Debug.Trace     as Debug

data Pulse
  = High
  | Low
  deriving (Show, Eq)

data FState
  = On
  | Off
  deriving (Show, Eq)

type CState = Map.Map String Pulse

type Destinations = [String]

data Module
  = FlipFlop
      { fs           :: FState
      , destinations :: Destinations
      }
  | Conjunction
      { cs           :: CState
      , destinations :: Destinations
      }
  | BroadCaster
      { destinations :: Destinations
      }
  deriving (Show, Eq)

parseInput input = parseLine <$> lines input
  where
    parseLine l =
      let [s, d] = Split.splitOn " -> " l
          destinations = Split.splitOn ", " d
       in case s of
            '%':label -> (label, FlipFlop Off destinations)
            '&':label -> (label, Conjunction Map.empty destinations)
            label     -> (label, BroadCaster destinations)

wireUpModules (modules :: [(String, Module)]) =
  foldl wireUp (Map.fromList modules) conjunctions
  where
    conjunctions =
      filter
        (\(_, m) ->
           case m of
             Conjunction _ _ -> True
             _               -> False)
        modules
    wireUp mm (x, c) =
      let sources =
            Map.fromList
              ((, Low) . fst <$>
               filter (\(_, m) -> elem x $ destinations m) modules)
       in Map.insert x (Conjunction sources (destinations c)) mm

part1 (moduleMap :: Map.Map String Module) =
  (!! 1000) $ iterate pressButton (0, 0, moduleMap)
  where
    pressButton (lowSent, highSent, mm :: Map.Map String Module) =
      let (BroadCaster d) = mm Map.! "broadcaster"
          newPulses = ("broadcaster", Low, ) <$> d
          (newLow, newHigh) = countPulses (lowSent + 1) highSent newPulses
       in propagate newPulses (newLow, newHigh, mm)
    countPulses lowSent highSent sent =
      let (low, high) = List.partition (\(_, p, _) -> p == Low) sent
       in (lowSent + length low, highSent + length high)
    propagate [] r = r
    propagate ((src, pulse :: Pulse, x):xs) (lowSent, highSent, mm) =
      case Map.lookup
             x -- (Debug.trace (src ++ " -" ++ show pulse ++ "-> " ++ x) x)
             mm of
        Nothing -> propagate xs (lowSent, highSent, mm)
        Just m ->
          case (pulse, m) of
            (High, FlipFlop _ _) -> propagate xs (lowSent, highSent, mm)
            -- If it was off, it turns on and sends a high pulse.
            (Low, FlipFlop Off d) ->
              let newPulses = ((x, High, ) <$> d)
                  (newLow, newHigh) = countPulses lowSent highSent newPulses
               in propagate
                    (xs ++ newPulses)
                    (newLow, newHigh, Map.insert x (FlipFlop On d) mm)
            -- If it was on, it turns off and sends a low pulse.
            (Low, FlipFlop On d) ->
              let newPulses = ((x, Low, ) <$> d)
                  (newLow, newHigh) = countPulses lowSent highSent newPulses
               in propagate
                    (xs ++ newPulses)
                    (newLow, newHigh, Map.insert x (FlipFlop Off d) mm)
            -- When a pulse is received, the conjunction module first updates its memory for that input.
            -- Then, if it remembers high pulses for all inputs, it sends a low pulse; otherwise, it sends a high pulse.
            (p, Conjunction s d) ->
              let s' = Map.insert src pulse s
                  newPulse =
                    if all (\(_, p) -> p == High) (Map.toList s')
                      then Low
                      else High
                  newPulses = ((x, newPulse, ) <$> d)
                  (newLow, newHigh) = countPulses lowSent highSent newPulses
               in propagate
                    (xs ++ newPulses)
                    (newLow, newHigh, Map.insert x (Conjunction s' d) mm)

main = do
  input <- readFile "input.txt"
  let modules = parseInput input
  let moduleMap = wireUpModules modules
  putStrLn "Part 1"
  let (low, high, _) = part1 moduleMap
  print $ low * high
