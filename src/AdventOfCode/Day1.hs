module AdventOfCode.Day1 where

import Data.List (tails)

readInputs :: FilePath -> IO [Int]
readInputs = fmap (fmap read . lines) . readFile

day1Part1 :: FilePath -> IO Int
day1Part1 file = do
  numbers <- readInputs file
  pure $
    head
      [ x * y
        | (x : xs) <- tails numbers,
          y <- xs,
          x + y == 2020
      ]

day1Part2 :: FilePath -> IO Int
day1Part2 file = do
  numbers <- readInputs file
  pure $
    head
      [ x * y * z
        | (x : xs) <- tails numbers,
          (y : ys) <- tails xs,
          z <- ys,
          x + y + z == 2020
      ]
