{- cabal:
     build-depends: base, parsec
-}

module Day2 where

import Text.Parsec
import Text.Parsec.String


-- Part 1

part1 :: FilePath -> IO Int
part1 fname =
  sum . map gameID . filter isPossible . listGames <$> readFile fname

colour :: GenParser Char s String
colour = try (string "red")
     <|> try (string "green")
     <|> try (string "blue")
  
singleExtractionParser :: GenParser Char s (Int, String)
singleExtractionParser =
  do n <- many1 digit
     spaces
     c <- colour
     return (read n, c)

extractionsParser :: GenParser Char s (Int, Int, Int)
extractionsParser = helper 0 0 0
  where
    helper r g b = try (oneOf " ," >> helper r g b)
               <|> try (singleExtractionParser >>= \(n, c) ->
                           case c of
                             "red"   -> helper (r+n) g b
                             "green" -> helper r (g+n) b
                             "blue"  -> helper r g (b+n)
                             _       -> undefined)
               <|> return (r, g, b)

data Game = Game { gameID      :: Int
                 , extractions :: [(Int, Int, Int)]
                 }
  deriving (Show)

gameParser :: GenParser Char s Game
gameParser =
  do _ <- string "Game"
     spaces
     n <- many1 digit
     spaces
     _ <- char ':'
     extrs <- extractionsParser `sepEndBy` char ';'
     return $ Game (read n) extrs

gamesParser :: GenParser Char s [Game]
gamesParser = gameParser `sepEndBy` newline

listGames :: String -> [Game]
listGames = either (\_ -> []) id . parse gamesParser ""

isPossible :: Game -> Bool
isPossible (Game _ triples) =
  all (\(r,g,b) -> r <= 12 && g <= 13 && b <= 14) triples


-- Part 2

part2 :: FilePath -> IO Int
part2 fname =
  sum . map gamePower . listGames <$> readFile fname 

gamePower :: Game -> Int
gamePower (Game _ extrs) = rmax * gmax * bmax
  where
    (rmax, gmax, bmax) =
      foldl1 (\(a,b,c) (a',b',c') -> (max a a', max b b', max c c')) extrs

