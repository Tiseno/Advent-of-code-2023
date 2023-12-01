import qualified Data.Char as Char

wordDigits :: String -> String
wordDigits xs@('o':'n':'e':_) = '1' : wordDigits (tail xs)
wordDigits xs@('t':'w':'o':_) = '2' : wordDigits (tail xs)
wordDigits xs@('t':'h':'r':'e':'e':_) = '3' : wordDigits (tail xs)
wordDigits xs@('f':'o':'u':'r':_) = '4' : wordDigits (tail xs)
wordDigits xs@('f':'i':'v':'e':_) = '5' : wordDigits (tail xs)
wordDigits xs@('s':'i':'x':_) = '6' : wordDigits (tail xs)
wordDigits xs@('s':'e':'v':'e':'n':_) = '7' : wordDigits (tail xs)
wordDigits xs@('e':'i':'g':'h':'t':_) = '8' : wordDigits (tail xs)
wordDigits xs@('n':'i':'n':'e':_) = '9' : wordDigits (tail xs)
wordDigits (x:xs)
  | Char.isDigit x = x : wordDigits xs
wordDigits (_:xs) = wordDigits xs
wordDigits [] = []

solution f input = sum $ (\l -> read [head l, last l]) . f <$> lines input

main = do
  input <- readFile "input.txt"
  putStrLn "Part 1"
  print $ solution (filter Char.isDigit) input
  putStrLn "Part 2"
  print $ solution wordDigits input
