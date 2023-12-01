{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Eta reduce" #-}
import qualified Data.Char as Char

parseInput input = lines input

part1 input = sum $ fmap f $ fmap (filter Char.isDigit) input
  where
    f l = read [head l, last l]

toOnlyDigits :: String -> String
toOnlyDigits ('o':'n':'e':xs) = '1' : toOnlyDigits xs
toOnlyDigits ('t':'w':'o':xs) = '2' : toOnlyDigits xs
toOnlyDigits ('t':'h':'r':'e':'e':xs) = '3' : toOnlyDigits xs
toOnlyDigits ('f':'o':'u':'r':xs) = '4' : toOnlyDigits xs
toOnlyDigits ('f':'i':'v':'e':xs) = '5' : toOnlyDigits xs
toOnlyDigits ('s':'i':'x':xs) = '6' : toOnlyDigits xs
toOnlyDigits ('s':'e':'v':'e':'n':xs) = '7' : toOnlyDigits xs
toOnlyDigits ('e':'i':'g':'h':'t':xs) = '8' : toOnlyDigits xs
toOnlyDigits ('n':'i':'n':'e':xs) = '9' : toOnlyDigits xs
toOnlyDigits (x:xs)
  | Char.isDigit x = x : toOnlyDigits xs
toOnlyDigits (_:xs) = toOnlyDigits xs
toOnlyDigits [] = []

toOnlyDigitsRR :: String -> String
toOnlyDigitsRR input = reverse $ toOnlyDigitsR $ reverse input
  where
    toOnlyDigitsR :: String -> String
    toOnlyDigitsR ('e':'n':'o':xs) = '1' : toOnlyDigitsR xs
    toOnlyDigitsR ('o':'w':'t':xs) = '2' : toOnlyDigitsR xs
    toOnlyDigitsR ('e':'e':'r':'h':'t':xs) = '3' : toOnlyDigitsR xs
    toOnlyDigitsR ('r':'u':'o':'f':xs) = '4' : toOnlyDigitsR xs
    toOnlyDigitsR ('e':'v':'i':'f':xs) = '5' : toOnlyDigitsR xs
    toOnlyDigitsR ('x':'i':'s':xs) = '6' : toOnlyDigitsR xs
    toOnlyDigitsR ('n':'e':'v':'e':'s':xs) = '7' : toOnlyDigitsR xs
    toOnlyDigitsR ('t':'h':'g':'i':'e':xs) = '8' : toOnlyDigitsR xs
    toOnlyDigitsR ('e':'n':'i':'n':xs) = '9' : toOnlyDigitsR xs
    toOnlyDigitsR (x:xs)
      | Char.isDigit x = x : toOnlyDigitsR xs
    toOnlyDigitsR (_:xs) = toOnlyDigitsR xs
    toOnlyDigitsR [] = []

part2 :: [String] -> Int
part2 input = sum $ fmap f input
  where
    f l = read [head (toOnlyDigits l), last (toOnlyDigitsRR l)] :: Int

main = do
  input <- readFile "input.txt"
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
  putStrLn "Part 2"
  print $ part2 parsed
