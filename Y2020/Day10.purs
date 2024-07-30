module Y2020.Day10
  ( main
  , run
  ) where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.Foldable as F
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple, uncurry)
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)





part_1 :: Array Int -> Int
part_1 = A.sort
    >>> (identity &&& A.cons 0)
    >>> uncurry (A.zipWith sub)
    >>> F.foldl go { a: 0, b: 1 }
    >>> (_.a * _.b)

  where

    go r@{ a, b } n = case n of

        1 -> r { a = a + 1 }
        3 -> r { b = b + 1 }
        _ -> r





part_2 :: Array Int -> String
part_2 arr = A.drop 1 arr'
           # F.foldl go cache
           # aggr

  where

    cache = M.singleton zero (one :: BigInt)

    go acc n = fromMaybe acc $ M.insert n <$> sum <@> acc
      where

        sum = query max n arr'
            # traverse (flip M.lookup acc)
            # map F.sum

    aggr = M.lookup last
        >>> fromMaybe zero
        >>> BI.toString
        >>> (_ <> "n")

    max = 3

    init = arr # NA.cons' 0 >>> NA.sort
    last = init # NA.last >>> (_ + max)
    arr' = NA.toArray $ NA.snoc init last

    query m hi = A.filter $ between (hi - m) (hi - 1)





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> Tuple Int String
run = S.trim >>> lines >>> A.mapMaybe fromString >>> (part_1 &&& part_2)

