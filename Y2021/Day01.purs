module Y2021.Day01
  ( main
  , run
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (class Foldable)
import Data.Foldable as FD
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple (fst, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (str_to_arr_int)





part_one :: forall f. Foldable f => f Int -> Int
part_one = fst <<< FD.foldl (uncurry go) (-one /\ zero)
  where
    go acc prev n | n > prev = (acc + one) /\ n
                  | otherwise = acc /\ n





part_two :: forall f. Foldable f => Int -> f Int -> Int
part_two n = FD.length
    <<< A.filter (uncurry (<))
    <<< A.zipWith (/\) `ap` A.drop n
    <<< A.fromFoldable





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> Int /\ Int
run = S.trim
    >>> str_to_arr_int
    >>> (part_one &&& part_two 3)

