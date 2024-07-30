module Y2022.Day11
  ( main
  , run
  ) where

import Prelude

import Data.Array as A
import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.Either (fromRight)
import Data.Foldable (foldMap, foldl, length, product)
import Data.FoldableWithIndex (class FoldableWithIndex, foldWithIndexM)
import Data.Function (applyN, on)
import Data.Int (toNumber)
import Data.Lens ((+~), (.~), (<>~), (^?))
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (alaF)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (read_int)
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





data Exp a
  = Old
  | Num a

type Monkey =
  { divisible :: BigInt
  , operation :: BigInt  -> BigInt
  , decision  :: Boolean -> Int
  , items     :: Array BigInt
  }

type Queue = Map Int (Array BigInt)
type Count = Array Int
type Cache = Queue /\ Count





part_one :: forall f. Functor f => FoldableWithIndex Int f =>
        f Monkey -> Int
part_one = let
    n3 = BI.fromInt 3
  in
    gen 20 (_ / n3) >>> product





part_two :: forall f. Functor f => FoldableWithIndex Int f =>
        f Monkey -> Number
part_two xs = let
    ms = xs <#> _.divisible # product
  in
    xs # gen 10_000 (_ `mod` ms)
       # alaF Multiplicative foldMap toNumber





gen :: forall f. Functor f => FoldableWithIndex Int f =>
        Int -> (BigInt -> BigInt) -> f Monkey -> Array Int
gen n fn xs = acc # tp # go # snd # A.sort # A.takeEnd 2
  where

    fs = xs <#> mk fn

    go = applyN (loop fs) n

    acc = xs <#> _.items # M.fromFoldableWithIndex

    tp = Tuple <*> (length >>> flip A.replicate zero)

    mk by { divisible, operation, decision } i = k /\ v
      where v = by $ operation $ i
            m = v `mod` divisible
            k = decision $ m == zero





loop :: forall f. FoldableWithIndex Int f =>
        f (BigInt -> Int /\ BigInt) -> Cache -> Cache
loop fab acc = fromMaybe acc $ foldWithIndexM go acc fab
  where

    move (k /\ n) = ix k <>~ pure n

    go i (queue /\ count) fn = ado

        xs <- queue ^? ix i

        let rs = fn <$> xs
            c  = count   #   ix i   +~   length xs
            q' = queue   #   ix i   .~   mempty
            q  = foldl (flip move) q' rs

        in q /\ c





main :: Effect Unit
main = Console.logShow 42





run :: String -> (Int /\ Number)
run = S.trim >>> parse >>> (part_one &&& part_two)
  where

    parse = runParser reading >>> fromRight mempty

    reading = flip C.sepEndBy P.whiteSpace ado

        lead "Monkey " *> read_int <* P.char ':'

        _     <- lead "Starting items: "
        items <- A.fromFoldable <$> C.sepBy read_big_int (P.string ", ")

        _         <- lead "Operation: new = "
        operation <- eval <$> exp <*> ops <*> exp

        _         <- lead "Test: divisible by "
        divisible <- read_big_int

        t <- lead  "If true: throw to monkey " *> read_int
        f <- lead "If false: throw to monkey " *> read_int

        let decision = if _ then t else f

        in { items, operation, divisible, decision }

    lead = void <<< C.manyTill P.anyChar <<< P.string

    read_big_int = read_int <#> BI.fromInt

    exp = C.choice
      [ P.string "old" $> Old
      , read_big_int  <#> Num
      ]

    ops = C.between P.whiteSpace P.whiteSpace $ C.choice
      [ P.char '*' $> (*)
      , P.char '+' $> (+)
      ]

    eval lhs fn rhs n = lhs `op` rhs
      where op = fn `on` case _ of
                Old   -> n
                Num m -> m

