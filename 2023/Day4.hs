{- cabal:
     build-depends: base, parsec
-}

module Day4 where

import Text.Parsec
import Text.Parsec.String


-- Part 1

data ScratchCard = ScratchCard Int [Int] [Int]
  deriving (Show)

part1 :: FilePath -> IO Int
part1 fname = sum . map worth . listScratchCards <$> readFile fname

numberParser :: GenParser Char s Int
numberParser = read <$> many1 digit

scratchCardParser :: GenParser Char s ScratchCard
scratchCardParser =
  do gameID  <- string "Card" >> spaces
                >> numberParser <* char ':'
     spaces
     winning <- numberParser `sepEndBy` many (oneOf " ")
     char '|' >> spaces
     yourNumbers <- numberParser `sepEndBy` many (oneOf " ")
     return $ ScratchCard gameID  winning yourNumbers

scratchCardsParser :: GenParser Char s [ScratchCard]
scratchCardsParser = scratchCardParser `sepEndBy` newline

listScratchCards :: String -> [ScratchCard]
listScratchCards = either (\_ -> []) id . parse scratchCardsParser ""

worth :: ScratchCard -> Int
worth (ScratchCard _ winning yourNumbers) =
  let l = length $ filter (`elem` winning) yourNumbers in
    if l >= 1
       then 2^(l-1)
       else 0


-- Part 2

part2 :: FilePath -> IO Int
part2 = undefined

