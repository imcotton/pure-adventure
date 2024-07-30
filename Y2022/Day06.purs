module Y2022.Day06
  ( main
  , run
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Enum (fromEnum)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class.Console as Console





part_by :: Int -> String -> Int
part_by n xs = fromMaybe zero $ tailRecM go init
  where

    init = { i: n, diff, cache }
      where diff = cache # L.nub # L.length
            cache = xs # SCU.take n # SCU.toCharArray # L.fromFoldable

    m = SCU.length xs

    go ref |    ref.i >= m = Nothing
           | ref.diff >= n = pure $ Done ref.i
           |     otherwise = ado

        { head, tail } <- L.uncons ref.cache

        next <- SCU.charAt ref.i xs

        let i = ref.i + one

            cache = L.snoc tail next

            diff = ref.diff
              - (fromEnum $ head `not L.elem` tail)
              + (fromEnum $ next `not L.elem` tail)

        in Loop { i, diff, cache }





main :: Effect Unit
main = Console.logShow 42





run :: String -> Int /\ Int
run = S.trim >>> (part_by 4 &&& part_by 14)

