parseInput input = lines input

part1 input = input

main = do
  input <- readFile "input.txt"
  input <- readFile "input.example.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
