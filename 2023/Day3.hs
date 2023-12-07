{- cabal:
     build-depends: base, parsec
-}

module Day3 where

import Text.Parsec
import Text.Parsec.String


-- Part 1

part1 :: FilePath -> IO Int
part1 = undefined

symbolParser :: GenParser Char s ()
symbolParser = noneOf ".123456789" >> return ()

numberParser :: GenParser Char s Int
numberParser = read <$> many1 digit


-- Part 2

part2 :: FilePath -> IO Int
part2 = undefined
