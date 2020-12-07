{-# LANGUAGE ScopedTypeVariables #-}

module AdventOfCode.Day5 (day5Part1, day5Part2) where

import Data.List (foldl', sort)

seatId :: String -> Int
seatId = foldl' (\a d -> 2 * a + d) 0 . fmap toBin
  where
    toBin 'F' = 0
    toBin 'B' = 1
    toBin 'L' = 0
    toBin 'R' = 1

day5Part1 :: FilePath -> IO Int
day5Part1 path = do
  seatIds <- fmap seatId . lines <$> readFile path
  pure $ foldl' max 0 seatIds

day5Part2 :: FilePath -> IO (Maybe Int)
day5Part2 path = do
  seatIds <- sort . fmap seatId . lines <$> readFile path
  pure $ solve seatIds Nothing
  where
    solve (x : y : xs) _
      | y /= x + 1 = Just (x + 1)
      | otherwise = solve (y : xs) Nothing
    solve _ _ = Nothing
