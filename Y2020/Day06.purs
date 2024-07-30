module Y2020.Day06
  ( run
  ) where

import Prelude

import Data.Array as A
import Data.Foldable as F
import Data.Maybe (maybe)
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple)
import Lib.Common (lines)





list :: (Set Char -> Set Char -> Set Char) -> String -> Int
list by str = str # S.trim
    >>> S.split (S.Pattern "\n\n")
    >>> map (lines >>> map (SCU.toCharArray >>> Set.fromFoldable))
    >>> map (count by >>> F.length)
    >>> F.sum

  where

    count op = A.uncons >>> maybe mempty \r -> F.foldl op r.head r.tail





run :: String -> Tuple Int Int
run = list Set.union
  &&& list Set.intersection

