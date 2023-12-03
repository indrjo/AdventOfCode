{- cabal:
     build-depends: base, parsec
-}

module Day2Part1 where

import Text.Parsec
import Text.Parsec.String

part1 :: FilePath -> IO Int
part1 fname =
  sum . map gameID . filter isPossible . listGames <$> readFile fname

listGames :: String -> [Game]
listGames = either (\_ -> []) id . parse gamesParser ""

colour :: GenParser Char s String
colour = try (string "red")
         <|> try (string "green")
         <|> try (string "blue")
  
singleExtractionParser :: GenParser Char s (Int, String)
singleExtractionParser =
  do n <- read <$> many1 digit
     spaces
     c <- colour
     return (n, c)

extractionsParser :: GenParser Char s (Int, Int, Int)
extractionsParser = helper 0 0 0
  where
    helper r g b = try (do spaces
                           (n, c) <- singleExtractionParser
                           case c of
                             "red"   -> helper (r+n) g b
                             "green" -> helper r (g+n) b
                             "blue"  -> helper r g (b+n))
                   <|> try (spaces >> char ',' >> helper r g b)
                   <|> return (r, g, b)

data Game = Game { gameID      :: Int
                 , extractions ::[(Int, Int, Int)]
                 }
  deriving (Show)

gameParser :: GenParser Char s Game
gameParser =
  do string "Game" >> spaces
     n <- read <$> many1 digit
     char ':'
     extrs <- extractionsParser `sepEndBy` char ';'
     return $ Game n extrs

gamesParser :: GenParser Char s [Game]
gamesParser = gameParser `sepEndBy` newline

isPossible :: Game -> Bool
isPossible (Game _ triples) =
  all (\(r, g, b) -> r <= 12 && g <= 13 && b <= 14) triples

