module Lib.Common
  ( try_read_file
  , read_file
  , str_to_arr_int
  , lines
  , words
  , chars_to_str
  , split_by
  , read_int
  , replace_all
  )
where

import Prelude

import Data.Array as A
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Int (fromString)
import Data.Int as I
import Data.Maybe (fromMaybe, maybe)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.String.Regex (regex, split)
import Data.String.Regex.Flags (multiline, noFlags)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Exception (Error, try)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import StringParser (Parser, fail)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





read_file :: String -> Effect String
read_file = readTextFile UTF8

try_read_file :: String -> Effect (Either Error String)
try_read_file = try <<< read_file





str_to_arr_int :: String -> Array Int
str_to_arr_int str = str # lines # A.mapMaybe fromString





foreign import replace_all :: String -> String -> String -> String





lines :: String -> Array String
lines str = case regex """\r?\n""" multiline of
    Left _ -> [ str ]
    Right reg -> split reg str





split_by :: String -> String -> String /\ String
split_by = flip split_by_at zero

split_by_at :: String -> Int -> String -> String /\ String
split_by_at p i = ado
    b  <- Tuple <*> identity
    ab <- flip SCU.splitAt
    fa <- flip SCU.indexOf' i $ S.Pattern p
    let fb = fa <#> ab <#> \{ before, after } -> before /\ after
        fc = fb <#> rmap (SCU.drop $ SCU.length p)
  in
    fromMaybe b fc





words :: String -> Array String
words str = case regex """\s+""" noFlags of
    Left _ -> [ str ]
    Right reg -> split reg str





chars_to_str :: forall f. Foldable f => f Char -> String
chars_to_str = SCU.fromCharArray <<< A.fromFoldable





read_int :: Parser Int
read_int = C.many1 P.anyDigit
    >>= chars_to_str
    >>> I.fromString
    >>> fail "invalid" `maybe` pure

