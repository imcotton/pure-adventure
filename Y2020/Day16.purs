module Y2020.Day16
  ( main
  , run
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array ((!!))
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Either (hush)
import Data.Filterable (class Filterable)
import Data.Filterable as FT
import Data.Foldable (class Foldable)
import Data.Foldable as FD
import Data.HeytingAlgebra (ff)
import Data.Int as I
import Data.Maybe (maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits as P
import Text.Parsing.StringParser.Combinators as C





type Ticket = Array Int

type Column =
  { label :: String
  , check :: Int -> Boolean
  }




part_1 :: forall f.
    Traversable f =>
        NonEmpty f Ticket ->
        f Column ->
        Int
part_1 ft fc = let
    by = fc <#> _.check # some # not
  in
    FD.sum $ FT.filter by $ flat ft






part_2 :: forall f.
    Traversable f =>
    Filterable f =>
        NonEmpty f Ticket ->
        f Column ->
        Int
part_2 (my :| ft) fc = let
    by = fc <#> _.check # some # FD.all
    ft' = FT.filter by ft
  in
    42








flat :: forall f a. Foldable f => Monoid a => f a -> a
flat = FD.foldl (<>) mempty

some :: forall f a. Foldable f => HeytingAlgebra a => f a -> a
some = FD.foldl (||) ff








mk_column :: Parser Column
mk_column = do

    label <- c2s <$> P.anyLetter `C.many1Till` P.char ':'

    P.skipSpaces

    fst <- range

    P.skipSpaces *> P.string "or" *> P.skipSpaces

    snd <- range

    let check = within fst || within snd

    pure { label, check }

  where

    within = uncurry between

    c2s = SCU.fromCharArray <<< A.fromFoldable

    int = C.many1 P.anyDigit
        >>= c2s
        >>> I.fromString
        >>> fail "invalid" `maybe` pure

    range = do

        lo <- int
        _  <- P.char '-'
        hi <- int

        guard $ lo <= hi
        pure  $ lo /\ hi







ex :: String
ex = """
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
"""



main :: Effect Unit
main = do
    Console.logShow $ run ex






run :: String -> Int /\ Int
run = S.trim
    >>> ("head\n" <> _)
    >>> S.split (S.Pattern "\n\n")
    >>> map lines
    >>> map (A.drop 1)
    >>> (\arr -> do

        a0 <- arr !! 0
        a1 <- arr !! 1
        a2 <- arr !! 2

        let tickets = a1 <> a2
                <#> S.split (S.Pattern ",")
                >>> A.mapMaybe I.fromString

            read = runParser mk_column >>> hush

        Tuple <$> (NA.toNonEmpty <$> NA.fromFoldable tickets)
              <@> (A.mapMaybe read a0)
    )
    >>> maybe (0 /\ 0) (uncurry part_1 &&& uncurry part_2)

