{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Day4 (day4Part1, day4Part2) where

import Control.Applicative (many, (<|>))
import Control.Monad (guard)
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser, sepBy, try, withRecovery)
import Text.Megaparsec.Char (char, digitChar, hexDigitChar, lowerChar, spaceChar, string)

type PassportValue = (String, String)

data MeasurementUnit = Inches | Centimeters deriving (Eq, Show)

data Height = Height Int MeasurementUnit deriving (Show, Eq)

data ValidPassport = ValidPassport
  { pByr :: Int,
    pIyr :: Int,
    pEyr :: Int,
    pHgt :: Height,
    pHcl :: String,
    pEcl :: String,
    pPid :: String,
    pCid :: Maybe String
  }
  deriving (Show, Eq)

type Parser = Parsec Void String

requiredFields :: Set String
requiredFields =
  Set.fromList
    [ "byr", -- (Birth Year)
      "iyr", -- (Issue Year)
      "eyr", -- (Expiration Year)
      "hgt", -- (Height)
      "hcl", -- (Hair Color)
      "ecl", -- (Eye Color)
      "pid" -- (Passport ID)
    ]

passportValue :: Parser PassportValue
passportValue = do
  key <- many lowerChar <* char ':'
  value <- many (char '#' <|> lowerChar <|> digitChar) <* try spaceChar

  pure (key, value)

validPassport :: Parser ValidPassport
validPassport = do
  values <- many passportValue
  guard $ allFieldsPresent values

  let byr = parseField values (parseInt 1920 2002) "byr"
      iyr = parseField values (parseInt 2010 2020) "iyr"
      eyr = parseField values (parseInt 2020 2030) "eyr"
      hgt = parseField values parseHeight "hgt"
      hcl = parseField values parseHcl "hcl"
      ecl = parseField values parseEcl "ecl"
      pid = parseField values parsePid "pid"
      cid = pure (snd <$> find ((== "pid") . fst) values)

  ValidPassport
    <$> byr
    <*> iyr
    <*> eyr
    <*> hgt
    <*> hcl
    <*> ecl
    <*> pid
    <*> cid
  where
    parseField values parser field = do
      case runParser parser "" . snd <$> find ((== field) . fst) values of
        Nothing -> fail ("Could not parse field '" <> field <> "'")
        Just parsed -> case parsed of
          Left e -> fail (errorBundlePretty e)
          Right v -> pure v

    parseInt :: Int -> Int -> Parser Int
    parseInt minValue maxValue = do
      ns <- many digitChar
      let value = read ns
      guard (value >= minValue && value <= maxValue)

      pure value

    parseHeight :: Parser Height
    parseHeight = do
      hgt <- read <$> many digitChar
      unit <-
        ( \case
            "cm" -> Centimeters
            _ -> Inches
          )
          <$> (string "cm" <|> string "in")
      guard
        ( if unit == Centimeters
            then hgt >= 150 && hgt <= 193
            else hgt >= 59 && hgt <= 76
        )

      pure (Height hgt unit)

    parseHcl :: Parser String
    parseHcl = do
      hash <- char '#'
      color <- many hexDigitChar
      guard (length color == 6)

      pure (hash : color)

    parseEcl :: Parser String
    parseEcl =
      string "amb"
        <|> string "blu"
        <|> string "brn"
        <|> string "gry"
        <|> string "grn"
        <|> string "hzl"
        <|> string "oth"

    parsePid :: Parser String
    parsePid = do
      pid <- many digitChar
      guard (length pid == 9)

      pure pid

    allFieldsPresent =
      (== Set.empty) . Set.difference requiredFields . Set.fromList . fmap fst

day4Part1 :: FilePath -> IO Int
day4Part1 = fmap (length . filter isPassportValid . parsePassports) . readFile
  where
    parsePassports raw =
      case runParser passports "" raw of
        Left _ -> error "Failed to parse passports"
        Right ps -> ps

    isPassportValid =
      (== Set.empty)
        . Set.difference requiredFields
        . Set.fromList
        . fmap fst

    passports = many passportValue `sepBy` char '\n'

day4Part2 :: FilePath -> IO Int
day4Part2 = fmap parsePassports . readFile
  where
    parsePassports raw =
      case runParser validPassportCount "" raw of
        Left _ -> error "Failed to parse passports"
        Right ps -> ps

    validPassportCount = do
      validated <- withRecovery (const $ pure 0) (1 <$ validPassport) `sepBy` char '\n'
      pure (sum validated)
