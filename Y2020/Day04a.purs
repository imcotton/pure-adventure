module Y2020.Day04a
  ( run
  , loop
  , mk_table
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CodeUnits as SU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Lib.Common (words)





type Passport

  = { byr :: Int          -- ^ Birth Year
    , iyr :: Int          -- ^ Issue Year
    , eyr :: Int          -- ^ Expiration Year
    , hgt :: String       -- ^ Height
    , hcl :: String       -- ^ Hair Color
    , ecl :: String       -- ^ Eye Color
    , pid :: String       -- ^ Passport ID
    , cid :: Maybe String -- ^ Country ID
    }





loop :: forall a. (String -> Maybe a) -> String -> Int
loop verify origin = go 0 0

  where

    nn = "\n\n"
    file = origin # S.trim # (_ <> nn)
    search_from i = SU.indexOf' (S.Pattern nn) i file
    slice_between l r = SU.slice l r file

    check l = do
        r <- search_from l
        chunk <- slice_between l r
        let count = Tuple (r + 1)
        verify chunk $> count 1 <|> pure (count 0)

    go i acc = case check i of
        Nothing -> acc
        Just (Tuple next n) -> go next (acc + n)





mk_passport :: String -> Maybe Passport
mk_passport draft = do

    table <- mk_table draft

    let get_str = table # flip M.lookup
        get_int = fromString <=< get_str

    byr <- get_int "byr"
    iyr <- get_int "iyr"
    eyr <- get_int "eyr"

    hgt <- get_str "hgt"
    hcl <- get_str "hcl"
    ecl <- get_str "ecl"
    pid <- get_str "pid"

    let cid = get_str "cid"

    pure { byr, iyr, eyr, hgt, hcl, ecl, pid, cid }





mk_table :: String -> Maybe (M.Map String String)
mk_table = S.trim
    >>> words
    >>> traverse mk_tuple
    >>> map M.fromFoldable





mk_tuple :: String -> Maybe (Tuple String String)
mk_tuple str = Tuple <$> arr !! 0 <*> arr !! 1
  where
    arr = S.split (S.Pattern ":") str





run :: String -> Int
run = loop mk_passport

