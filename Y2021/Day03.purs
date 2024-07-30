module Y2021.Day03
  ( main
  , run
  ) where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable)
import Data.Foldable as FD
import Data.Int as I
import Data.Int.Bits (shl, zshr, (.&.))
import Data.Maybe (maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor.Strong ((&&&), (***))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)





part_one :: forall f. Foldable f => NonEmpty f String -> Int
part_one (x :| xs) = let

    l = 1 /\ 0
    r = 0 /\ 1

    mk = SCU.toCharArray >>> map \s -> if s == '0' then l else r

    go acc s = A.zipWith (+) acc $ mk s

    to_int = flip FD.foldl 0 $ (+) <<< (*) 2

  in FD.foldl go (mk x) xs
    # map (uncurry (<))
    # map fromEnum &&& map not
    # identity *** map fromEnum
    # to_int *** to_int
    # uncurry (*)





part_two :: forall f. Foldable f => NonEmpty f String -> Int
part_two s@(x :| _) = let

    mk_marks = shl one <<< (_ - one)

    move_idx = flip zshr one

    from_bin = I.fromStringAs I.binary

    test b a = b == (a .&. b)

    go _ [   ] _ = zero
    go _ [ n ] _ = n
    go i   xs  b = let
        i' = move_idx i
        { yes, no } = A.partition (test i) xs
        select = if b then max else min
        xs' = snd $ sizing yes `select` sizing no
      in
        go i' xs' b

    fn = go (x # S.length # mk_marks)
            (s # A.fromFoldable # A.mapMaybe from_bin)

  in
    fn true * fn false






sizing :: forall f a. Foldable f => f a -> Int /\ f a
sizing = flip Tuple <*> FD.length





main :: Effect Unit
main = do
    traceM 42
    Console.logShow 42





run :: String -> Int /\ Int
run = S.trim
    >>> lines
    >>> map S.trim
    >>> (NA.fromFoldable >>> map NA.toNonEmpty)
    >>> maybe (0 /\ 0) (part_one &&& part_two)

