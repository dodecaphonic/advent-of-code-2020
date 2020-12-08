module AdventOfCode.Day6 where

import Control.Monad (join)
import Data.List (foldl')
import Data.Monoid (Sum (..))
import qualified Data.Set as Set

groups :: [String] -> [[String]]
groups = groupify []
  where
    groupify gs [] = gs
    groupify [] (x : xs) = groupify [[x]] xs
    groupify (cg : gs) (x : xs)
      | x /= "" = groupify ((x : cg) : gs) xs
      | otherwise = groupify ([] : cg : gs) xs

day6Part1 :: FilePath -> IO Int
day6Part1 =
  fmap
    ( getSum
        . foldMap (Sum . Set.size)
        . fmap (Set.fromList . join)
        . groups
        . lines
    )
    . readFile

day6Part2 :: FilePath -> IO Int
day6Part2 = fmap (getSum . foldMap everyoneAnswered . groups . lines) . readFile
  where
    everyoneAnswered [] = Sum 0
    everyoneAnswered (x : xs) =
      Sum
        ( Set.size
            ( foldl'
                ( \is p ->
                    Set.intersection
                      is
                      (Set.fromList p)
                )
                (Set.fromList x)
                xs
            )
        )
