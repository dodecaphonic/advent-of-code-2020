{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Day7 where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data Bag = Bag
  { bagName :: !String,
    containedBags :: ![ContainedBag]
  }
  deriving (Eq, Show)

data ContainedBag = ContainedBag
  { containedBag :: !String,
    holdsCount :: !Int
  }
  deriving (Eq, Show)

type Parser = Parsec Void String

bag :: Parser Bag
bag = do
  name <- bagNameP
  _ <- string " contain "
  contains <- [] <$ string "no other bags" <|> containedBagP `sepBy` string ", "
  _ <- char '.'

  pure (Bag name contains)
  where
    bagNameP :: Parser String
    bagNameP = do
      colorA <- many lowerChar <* space
      colorB <- many lowerChar <* space <* (string "bags" <|> string "bag")

      pure (colorA <> " " <> colorB)

    containedBagP :: Parser ContainedBag
    containedBagP = do
      n <- read <$> (many digitChar <* space)
      name <- bagNameP

      pure (ContainedBag name n)

parseRules :: String -> [Bag]
parseRules raw =
  let parsed = fmap (runParser bag "") (lines raw)
   in case sequenceA parsed of
        Left e -> error (errorBundlePretty e)
        Right bags -> bags

bagsContaining :: String -> [Bag] -> Set String
bagsContaining bagToSearch = search bagToSearch . foldl' buildGraph Map.empty
  where
    buildGraph :: Map String (Set String) -> Bag -> Map String (Set String)
    buildGraph graph containerBag =
      foldl'
        ( \graph' cb ->
            Map.alter
              ( \case
                  Nothing -> Just (Set.singleton (bagName containerBag))
                  Just bs -> Just (Set.insert (bagName containerBag) bs)
              )
              (containedBag cb)
              graph'
        )
        graph
        (containedBags containerBag)

    search name graph =
      let directContainers = fromMaybe Set.empty (Map.lookup name graph)
          indirectContainers = foldMap (`search` graph) directContainers
       in directContainers <> indirectContainers

day7Part1 :: FilePath -> IO Int
day7Part1 = (Set.size . bagsContaining "shiny gold" . parseRules <$>) . readFile

day7Part2 :: FilePath -> IO Int
day7Part2 = fmap solvePart2 . readFile
  where
    solvePart2 =
      getSum
        . containedBy "shiny gold"
        . foldMap (\b@(Bag name _) -> Map.singleton name b)
        . parseRules

    containedBy containerBag bags =
      case Map.lookup containerBag bags of
        Just (Bag _ cbags) ->
          foldMap
            ( \(ContainedBag cbagName holds) ->
                Sum holds + Sum holds * containedBy cbagName bags
            )
            cbags
        Nothing -> Sum 0
