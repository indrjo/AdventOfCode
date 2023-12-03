{- cabal:
    build-depends: base
-}

module Day1Part1 where

import Data.Char (isDigit)

part1 :: FilePath -> IO Int
part1 fname = sum . listCalibrationValues1 <$> readFile fname

calibrationValue :: String -> Int
calibrationValue string =
  let digits = filter isDigit string in
    read [head digits, last digits]

listCalibrationValues1 :: String -> [Int]
listCalibrationValues1 = map calibrationValue . lines

