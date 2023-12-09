parseInput input = fmap read . words <$> lines input

part1 (histories :: [[Int]]) = sum $ fmap extrapolateNext histories
  where
    extrapolateNext (history :: [Int]) = extrapolate $ allDerivatives history
    allDerivatives history
      | any (0 /=) history =
        let d = zipWith (-) (tail history) history
         in history : allDerivatives d
    allDerivatives history = [history]
    extrapolate histories = sum $ fmap last histories

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
