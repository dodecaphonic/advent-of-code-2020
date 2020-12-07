module AdventOfCode.Day3 (readInput, day3Part1, day3Part2) where

import Data.List (foldl')

type Terrain = [String]

readInput :: FilePath -> IO [String]
readInput = fmap lines . readFile

traverseTerrain :: (Int, Int) -> Terrain -> [Char]
traverseTerrain (sw, sh) terrain = move (0, 0) []
  where
    terrainWidth = length (head terrain)
    terrainHeight = length terrain
    move (pw, ph) acc =
      if (ph + sh) < terrainHeight
        then
          let nw = (pw + sw) `mod` terrainWidth
              nh = ph + sh
              atSpot = (terrain !! nh) !! nw
           in move (nw, nh) (atSpot : acc)
        else acc

treesAtStep :: (Int, Int) -> Terrain -> Int
treesAtStep step = length . filter (== '#') . traverseTerrain step

day3Part1 :: FilePath -> IO Int
day3Part1 = fmap (treesAtStep (3, 1)) . readInput

day3Part2 :: FilePath -> IO Int
day3Part2 file = do
  terrain <- readInput file
  pure $ foldl' (\acc step -> acc * treesAtStep step terrain) 1 steps
  where
    steps = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
