{- cabal:
     build-depends: base, parsec
     other-modules: Day2Part1
-}

module Day2Part2 where

import Data.List
import Day2Part1

part2 :: FilePath -> IO Int
part2 fname =
  sum . map gamePower . listGames <$> readFile fname 

gamePower :: Game -> Int
gamePower (Game _ extrs) = rmax * gmax * bmax
  where
    (rmax, gmax, bmax) =
      foldl1' (\(a,b,c) (a',b',c') -> (max a a', max b b', max c c')) extrs
