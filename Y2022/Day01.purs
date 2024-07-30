module Y2022.Day01
  ( main
  , run
  ) where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Foldable (class Foldable, maximumBy, sum)
import Data.Function (on)
import Data.Int as I
import Data.Maybe (fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)





part_one :: forall f. Foldable f => f Int -> Int
part_one = maximumBy compare >>> fromMaybe zero





part_two :: Int -> Array Int -> Int
part_two n = A.sortBy compare >>> A.takeEnd n >>> sum





main :: Effect Unit
main = Console.logShow 42





run :: String -> Int /\ Int
run = S.trim
    >>> lines
    >>> map S.trim
    >>> map (I.fromString >>> fromMaybe zero)
    >>> A.groupBy (eq `on` (eq zero))
    >>> A.filter (NA.head >>> not eq zero)
    >>> map NA.toArray
    >>> map sum
    >>> (part_one &&& part_two 3)

