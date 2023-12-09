parseInput input = fmap read . words <$> lines input

allDerivatives history
  | any (0 /=) history =
    let d = zipWith (-) (tail history) history
     in history : allDerivatives d
allDerivatives history = [history]

solve (histories :: [[Int]]) extrapolationFn =
  sum $ fmap (extrapolationFn . allDerivatives) histories

extrapolateNext histories = sum $ fmap last histories

extrapolatePrev histories = foldr (-) 0 $ fmap head histories

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ solve parsed extrapolateNext
  putStrLn "Part 2"
  print $ solve parsed extrapolatePrev
