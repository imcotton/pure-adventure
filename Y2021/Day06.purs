module Y2021.Day06
  ( main
  , run
  ) where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.Foldable as FD
import Data.Function (applyN)
import Data.Int as I
import Data.Map as M
import Data.Maybe (maybe)
import Data.Profunctor.Strong ((&&&), (***))
import Data.String as S
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console





part_one :: Array Int -> BigInt
part_one = step  80

part_two :: Array Int -> BigInt
part_two = step 256





step :: Int -> Array Int -> BigInt
step n = A.groupAll
    >>> map (NA.head &&& NA.length >>> BI.fromInt)
    >>> M.fromFoldable
    >>> applyN move n
    >>> FD.sum

  where

    move table = produce # maybe new (M.unionWith (+) new)

      where

        produce = ado
            v <- M.lookup zero table
          in
            M.fromFoldable [ 6 /\ v, 8 /\ v ]

        new = rest <#> lmap (_ - one) # M.fromFoldable

        rest :: Array (Int /\ BigInt)
        rest = table # M.delete zero # M.toUnfoldableUnordered






main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> String /\ String
run = S.trim
   >>> S.split (S.Pattern ",")
   >>> A.mapMaybe I.fromString
   >>> (part_one &&& part_two)
   >>> (to_str *** to_str)

  where

    to_str = BI.toString >>> (_ <> "n")

