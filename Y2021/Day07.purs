module Y2021.Day07
  ( main
  , run
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Plus (empty)
import Data.Array as A
import Data.Foldable (class Foldable)
import Data.Foldable as FD
import Data.Int as I
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class.Console as Console





part_one :: Array Int -> Int
part_one arr = let
    m = mid arr
    arr' = arr <#> (_ - m) >>> abs
  in
    FD.sum arr'





part_two :: Array Int -> Int
part_two = fromMaybe zero <<< FD.minimum <<< steps
  where

    steps arr = stepping arr <#> \n ->
        FD.sum $ adding n <$> arr

    adding a b = let
        n = abs $ a - b
      in
        n * (n + 1) / 2

    stepping = let
        gen = lift2 A.range <$> FD.minimum <*> FD.maximum
      in
        gen >>> fromMaybe empty





mid :: forall f. Foldable f => f Int -> Int
mid = A.fromFoldable >>> A.sort >>> mid'

mid' :: forall f. Foldable f => f Int -> Int
mid' arr = let
    two = one + one
    half = FD.length arr / two
    selected = (/) <$> meet arr <@> two
    meet = lift2 (+) <$> FD.indexl half
                     <*> FD.indexr half
  in
    zero `fromMaybe` selected





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> Int /\ Int
run = S.trim
   >>> S.split (S.Pattern ",")
   >>> A.mapMaybe I.fromString
   >>> (part_one &&& part_two)

