module Y2022.Day14
  ( main
  , run
  ) where

import Prelude

import Data.Array as A
import Data.Either (fromRight)
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable, fold, foldMap, maximumBy)
import Data.Function (on)
import Data.Lens (hasn't)
import Data.Lens.Index (ix)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set as St
import Data.String as S
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (read_int)
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





type Point = Int /\ Int

source = 500 /\ 0 :: Point





part_one :: Set Point -> Int
part_one sp = go zero (pure source) sp
  where

    floor = mk_floor sp

    go n xs acc = let
        n' = n + one
        leaked /\ xs' /\ acc' = drip floor xs acc
      in
        if leaked then n
                  else go n' xs' acc'





part_two :: Set Point -> Int
part_two sp = part_one $ sp <> ground
  where

    x /\ _ = source
    _ /\ y = mk_floor sp

    n = y + 2
    m = n * 2 + 3

    l = x - m / 2
    r = x + m / 2

    ground = mk_set [ (l /\ n) : (r /\ n) : Nil ]





drip :: Point -> List Point -> Set Point -> Boolean /\ List Point /\ Set Point
drip floor = go
  where

    go      Nil   acc = true /\ Nil /\ acc
    go xs@(x : s) acc = let
        next = move x acc
        leaked = next `on (>) snd` floor
      in
        if next == x || leaked then leaked /\ s /\ St.insert x acc
                               else go (next : xs) acc

    move pre xs = let
        free s = ix s `hasn't` xs # (fromEnum :: Boolean -> Int)
        lft /\ mid /\ rit = peak pre
      in
        case free lft, free mid, free rit of
            0, 0, 0 -> pre
            1, 0, _ -> lft
            0, 0, 1 -> rit
            _, _, _ -> mid

    peak u = let
        m = u + (0 /\ 1)
        l = m - (1 /\ 0)
        r = m + (1 /\ 0)
      in
        l /\ m /\ r





mk_floor :: forall f. Foldable f => f Point -> Point
mk_floor = maximumBy (comparing snd)
       >>> fromMaybe (top /\ top)





mk_set ::  forall f. Foldable f => f (List Point) -> Set Point
mk_set = foldMap go
  where

    go = (L.zipWith range <*> L.drop 1) >>> fold

    range a@(ax /\ ay) b@(bx /\ by)
        | a  == b   = St.singleton a
        | ax == bx  = St.fromFoldable (     Tuple ax <$> ay `A.range` by)
        | ay == by  = St.fromFoldable (flip Tuple ay <$> ax `A.range` bx)
        | otherwise = St.empty





main :: Effect Unit
main = Console.logShow 42





run :: String -> (Int /\ Int)
run = S.trim >>> parse >>> mk_set >>> (part_one &&& part_two)
  where

    parse = runParser (line `C.sepBy` P.whiteSpace) >>> fromRight mempty

    line = pair `C.sepBy` P.string " -> "

    pair = Tuple <$> read_int <* P.char ','
                 <*> read_int

