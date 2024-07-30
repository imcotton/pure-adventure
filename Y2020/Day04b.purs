module Y2020.Day04b
  ( run
  ) where

import Prelude

import Y2020.Day04a (loop, mk_table)
import Control.Alt ((<|>))
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), hush, note)
import Data.Int as I
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String as S
import Text.Parsing.StringParser (fail, runParser)
import Text.Parsing.StringParser.CodeUnits as P
import Text.Parsing.StringParser.Combinators ((<?>), choice)






type Passport

  = { byr :: Year          -- ^ Birth Year
    , iyr :: Year          -- ^ Issue Year
    , eyr :: Year          -- ^ Expiration Year
    , hgt :: Height        -- ^ Height
    , hcl :: HairColor     -- ^ Hair Color
    , ecl :: EyeColor      -- ^ Eye Color
    , pid :: PID           -- ^ Passport ID
    , cid :: Maybe String  -- ^ Country ID
    }





newtype Year = Year Int
derive newtype instance Show Year

newtype HairColor = HairColor String
derive newtype instance Show HairColor

newtype EyeColor = EyeColor String
derive newtype instance Show EyeColor

newtype PID = PID String
derive newtype instance Show PID

data Height = Height Int String
instance Show Height where
   show (Height n s) = show n <> s





mk_passport :: String -> Either String Passport
mk_passport draft = do

    table <- mk_table draft # note "wrong table"

    let get_str' = table # flip M.lookup
        get_str = get_str' >>> note "wrong String"
        get_int = (get_str' >=> I.fromString) >>> note "wrong Int"

    byr <- get_int "byr" >>= mk_Year 1920 2002
    iyr <- get_int "iyr" >>= mk_Year 2010 2020
    eyr <- get_int "eyr" >>= mk_Year 2020 2030

    hgt <- get_str "hgt" >>= \s ->
            mk_Height "cm" 150 193 s
        <|> mk_Height "in"  59  76 s

    hcl <- get_str "hcl" >>= mk_HairColor "#" 6
    ecl <- get_str "ecl" >>= mk_EyeColor "amb blu brn gry grn hzl oth"
    pid <- get_str "pid" >>= mk_PID 9

    let cid = get_str' "cid"

    pure { byr, iyr, eyr, hgt, hcl, ecl, pid, cid }





mk_Year :: Int -> Int -> Int -> Either String Year
mk_Year l r n
    | between l r n = Right (Year n)
    | otherwise = Left "out range"



mk_HairColor :: String -> Int -> String -> Either String HairColor
mk_HairColor p n str = bimap show HairColor result
  where
    rule = P.regex $ p <> "[0-9a-f]{" <> show n <> "}"
    result = flip runParser str $ rule <?> "wrong hair color " <> str



mk_EyeColor :: String -> String -> Either String EyeColor
mk_EyeColor p str = bimap show EyeColor result
  where
    rule = choice $ P.string <$> S.split (S.Pattern " ") p
    result = flip runParser str $ rule <?> "wrong eye color " <> str



mk_PID :: Int -> String -> Either String PID
mk_PID n str = bimap show PID result
  where
    rule = P.regex $ "^[0-9]{" <> show n <> "}$"
    result = flip runParser str $ rule <?> "wrong PID " <> str



mk_Height :: String -> Int -> Int -> String -> Either String Height
mk_Height u l r str = lmap show $ flip runParser str do

    num <- P.regex "[0-9]+" >>= \s -> case I.fromString s of
        Just n -> if between l r n
            then pure n
            else fail $ "out range" <> show [ l, n, r ]
        Nothing -> fail $ "wrong number " <> s

    unit <- P.string u

    pure $ Height num unit





run :: String -> Int
run = loop $ mk_passport >>> hush

