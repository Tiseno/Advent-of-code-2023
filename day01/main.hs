import qualified Data.Char as Char
import qualified Data.List as List

wordss =
  zip
    ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    (fmap Char.intToDigit [1 ..])

wordDigits :: String -> String
wordDigits [] = []
wordDigits (x:xs)
  | Char.isDigit x = x : wordDigits xs
wordDigits xs =
  (snd <$> filter (\(w, _) -> w `List.isPrefixOf` xs) wordss) ++
  wordDigits (tail xs)

solution f input = sum $ (\l -> read [head l, last l]) . f <$> lines input

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  print $ solution (filter Char.isDigit) input
  putStrLn "Part 2"
  print $ solution wordDigits input
