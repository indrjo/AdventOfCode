{- cabal:
    build-depends: base, parsec
    other-modules: Day1Part1
-}

module Day1Part2 where

import Text.Parsec
import Text.Parsec.String

import Day1Part1

part2 :: FilePath -> IO Int
part2 fname = sum . listCalibrationValues2 <$> readFile fname

digitInLetters :: GenParser Char s String
digitInLetters =
      try (string "oneight"   >> return "18")
  <|> try (string "one"       >> return "1")
  <|> try (string "twone"     >> return "21")
  <|> try (string "two"       >> return "2")
  <|> try (string "threeight" >> return "38")
  <|> try (string "three"     >> return "3")
  <|> try (string "four"      >> return "4")
  <|> try (string "fiveight"  >> return "58")
  <|> try (string "five"      >> return "5")
  <|> try (string "six"       >> return "6")
  <|> try (string "sevenine"  >> return "79")
  <|> try (string "seven"     >> return "7")
  <|> try (string "eightwo"   >> return "82")
  <|> try (string "eighthree" >> return "83")
  <|> try (string "eight"     >> return "8")
  <|> try (string "nineight"  >> return "98")
  <|> try (string "nine"      >> return "9")

translateParser :: GenParser Char s String
translateParser =
      (eof >> return [])
  <|> (digitInLetters >>=
        \d -> (d ++) <$> translateParser)
  <|> (anyChar >>= 
        \c -> (c :) <$> translateParser)
  
listCalibrationValues2 :: String -> [Int]
listCalibrationValues2 =
  either (\_ -> []) listCalibrationValues1 . parse translateParser ""
