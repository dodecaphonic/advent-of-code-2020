module AdventOfCode.Day2 (day2Part1, day2Part2) where

import Control.Applicative (many)
import Data.Void (Void)
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (char, digitChar, lowerChar, space)

type Parser = Parsec Void String

data PolicyAndPassword = PolicyAndPassword
  { policyRefIdxs :: !(Int, Int),
    policyChar :: !Char,
    corruptedPassword :: !String
  }
  deriving (Show, Eq)

policyAndPasswordP :: Parser PolicyAndPassword
policyAndPasswordP = do
  minC <- (read :: String -> Int) <$> many digitChar
  _ <- char '-'
  maxC <- (read :: String -> Int) <$> many digitChar
  space
  pchar <- lowerChar
  _ <- char ':' *> space
  password <- many lowerChar

  pure (PolicyAndPassword (minC, maxC) pchar password)

readInput :: FilePath -> IO [PolicyAndPassword]
readInput file = do
  rawPasswords <- lines <$> readFile file
  let parsed = traverse (runParser policyAndPasswordP "") rawPasswords

  pure $
    case parsed of
      Left f -> error ("failed to parse inputs" <> show f)
      Right ps -> ps

day2Part1 :: FilePath -> IO Int
day2Part1 file = do
  inputs <- readInput file

  pure $ length (filter isValid inputs)
  where
    isValid (PolicyAndPassword (atLeast, atMost) refChar pwd) =
      let countOf = length $ filter (== refChar) pwd
       in countOf >= atLeast && countOf <= atMost

day2Part2 :: FilePath -> IO Int
day2Part2 file = do
  inputs <- readInput file

  pure $ length (filter isValid inputs)
  where
    isValid (PolicyAndPassword (eitherAt, orAt) refChar pwd) =
      let atA = pwd !! (eitherAt - 1)
          atB = pwd !! (orAt - 1)
       in (atA == refChar && atB /= refChar)
            || (atA /= refChar && atB == refChar)
