module Y2022.Day15
  ( main
  , run
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Either (fromRight)
import Data.Filterable (filter)
import Data.Foldable (all, find, maximum, sum)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.NonEmpty as NL
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord (abs)
import Data.Profunctor.Strong ((&&&), (***))
import Data.String as S
import Data.Tuple (Tuple(..), fst, snd, swap, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (read_int)
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





type Point = Int /\ Int

type Pair =
  { sensor :: Point
  , beacon :: Point
  , dist   :: Int
  }





part_one :: List Pair -> Int
part_one = map mk >>> L.mapMaybe (cut 2_000_000) >>> gen >>> count
  where

    mk = _.dist &&& _.sensor

    count xs = xs <#> swap <#> uncurry (-) # sum

    gen r0 = let
        r1 = merge r0
        r2 = merge r1
      in
        if r1 == r2 then     r1
                    else gen r2

    merge = map order >>> L.sort >>> go mempty

    order ab@(a /\ b) = if a <= b then      ab
                                  else swap ab

    go acc   Nil    = ease acc
    go acc (x : xs) = go acc' no
      where (a /\ b) = x
            by (c /\ _) = c <= b + one
            { yes, no } = L.partition by xs
            b' = yes <#> snd # maximum # maybe b (max b)
            acc' = L.snoc acc (a /\ b')

    ease xs = xs # L.groupBy connection <#> aggregation
      where connection (_ /\ a) (b /\ _) = a == b
                                        || a == b - one
            aggregation = Tuple <$> NL.head >>> fst
                                <*> NL.last >>> snd





part_two :: List Pair -> Number
part_two fa = go fb
            # filter (uncurry \x y -> within x && within y)
            # find (flip all fb <<< not <<< covered)
            # maybe zero (uncurry \x y -> x * max_size + y)
  where

    max_size = 4_000_000.0

    within = zero `between` max_size

    covered (x /\ y) p = fromMaybe false (uncurry between <$> cut y p <@> x)

    fb = fa <#> (_.dist &&& _.sensor) >>> (toNumber *** toNumber *** toNumber)

    go xs = do

        p1@(_ /\ x1 /\ _) <- xs
        p2@(_ /\ x2 /\ _) <- xs

        guard $ x1 < x2

        let { bottom_right: l1 } = slope p1
            { bottom_left:  l2 } = slope p2

        pure $ l1 `cross` l2

    equation (x1 /\ y1) (x2 /\ y2) = let
        k = (y1 - y2) / (x1 - x2)
        b = y1 - (k * x1)
      in
        k /\ b

    cross l1 l2 = let
        k1 /\ b1 = uncurry equation l1
        k2 /\ b2 = uncurry equation l2
        x = (b2 - b1) / (k1 - k2)
        y = (k1 * x) + b1
      in
        x /\ y

    slope (dist /\ x /\ y) = let
        n = dist + one
        xu = x /\ (y - n)
        xd = x /\ (y + n)
        yl = (x - n) /\ y
        yr = (x + n) /\ y
      in {    top_left: xu /\ yl
        ,    top_right: xu /\ yr
        ,  bottom_left: xd /\ yl
        , bottom_right: xd /\ yr
        }





cut :: forall a. Ord a => Ring a => a -> a /\ a /\ a -> Maybe (a /\ a)
cut depth (r /\ x /\ y) = let
    n = abs $ depth - y
    h = (x - r + n) /\ (x + r - n)
    within = (y - r) `between` (y + r)
  in
    if within depth then Just h
                    else Nothing





main :: Effect Unit
main = Console.logShow 42





run :: String -> Int /\ Number
run = S.trim >>> parse >>> (part_one &&& part_two)
  where

    parse = runParser (line `C.sepBy` P.whiteSpace) >>> fromRight mempty

    line = ado

        sensor <- lead    "Sensor at" *> pair
        beacon <- lead "beacon is at" *> pair

        let dist = sensor `manhattan_distance_to` beacon

        in { sensor, beacon, dist }

    pair = Tuple <$> (P.string  " x=" *> num)
                 <*> (P.string ", y=" *> num)

    lead = void <<< C.manyTill P.anyChar <<< P.string

    num = read_int <|> read_int_negative

    read_int_negative = P.char '-' *> read_int <#> negate

    manhattan_distance_to (ax /\ ay) (bx /\ by) = dx + dy
      where dx = abs $ ax - bx
            dy = abs $ ay - by

