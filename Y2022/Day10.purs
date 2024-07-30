module Y2022.Day10
  ( main
  , run
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Bifunctor (rmap)
import Data.BigInt as BI
import Data.Either (fromRight)
import Data.Enum (fromEnum)
import Data.Foldable (foldMap, indexl, length, maximum, sum)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Dual (Dual(..))
import Data.Newtype (ala)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Traversable (class Foldable, class Traversable, scanl, traverse)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable1 as UF1
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (read_int, replace_all)
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





data Ops
  = Noop
  | Plus Int





part_one :: forall f. Foldable f => f Ops -> Int
part_one ops = fromMaybe zero $ multiple seq points
  where

    cmd = mk_ops_map ops
    seq = addition cmd range
    range = A.range 1 $ fromMaybe 1 $ maximum points
    points = UF1.iterateN 6 (40 + _) 20 :: Array _

    multiple m = traverse go >>> map sum
      where go n = (*) <$> indexl (n - one) m
                       <@> n





part_two :: forall f. Foldable f => f Ops -> String /\ String
part_two ops = print arr /\ fromMaybe mempty big
  where

    cmd = mk_ops_map ops
    seq = addition cmd range
    arr = show <$> fromEnum <$> A.zipWith (window 3) seq points
    big = arr # S.joinWith "" # BI.fromBase 2 <#> BI.toBase 36
    range = A.range 1 $ length points
    points = 40 `by` 6

    window n = between <*> add (n - one)

    by w h = ala Dual foldMap m
      where m = UF1.replicate1 h $ UF1.range one w :: Array _

    chop n = go mempty
      where go acc [] = acc
            go acc xs = go (acc <> [ A.take n xs ]) (A.drop n xs)

    print = chop 40 >>> map (S.joinWith "") >>> S.joinWith "\n"
        >>> "1" `replace_all` "#"
        >>> "0" `replace_all` "."





addition :: forall f. Traversable f => Map Int Int -> f Int -> f Int
addition m = scanl go one
  where

    go n i = acc n $ lookup i
    acc = maybe <*> add
    lookup = flip M.lookup m





mk_ops_map :: forall f. Foldable f => f Ops -> Map Int Int
mk_ops_map = A.fromFoldable
    >>> pair
    >>> map plus
    >>> M.fromFoldable
    >>> M.catMaybes

  where

    pair = A.zipWith (flip Tuple) <*> seq

    seq = scanl (flip go) one
      where go Noop = add $ one
            go    _ = add $ one + one

    plus = rmap case _ of
        Plus n -> Just n
        _      -> Nothing





main :: Effect Unit
main = Console.logShow 42





run :: String -> (Int /\ String)
run = S.trim >>> parse >>> (part_one &&& part_two) >>> rmap snd
  where

    parse = runParser line >>> fromRight mempty

    line = flip C.sepBy P.whiteSpace $ C.choice
      [ P.string "noop" $> Noop
      , P.string "addx" <* P.whiteSpace *> num <#> Plus
      ]

    num = read_int <|> read_int_negative

    read_int_negative = P.char '-' *> read_int <#> negate

