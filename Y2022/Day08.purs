module Y2022.Day08
  ( main
  , run
  ) where

import Prelude

import Control.Monad.State (execState, modify_)
import Control.Plus (empty)
import Data.Either (fromRight)
import Data.Foldable (class Foldable, foldMap, length)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int as I
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (alaF)
import Data.Ord.Max (Max(..))
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set, insert)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Traversable (scanl, scanr)
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import StringParser (fail, runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





part_one :: List (List Int) -> Int
part_one = collect >>> length





part_two :: List (List Int) -> Int
part_two = cross <*> collect





cross :: forall f. Foldable f => List (List Int) -> f (Int /\ Int) -> Int
cross xs = fromMaybe zero <<< alaF Max foldMap go
  where

    ys = L.transpose xs

    go (x /\ y) = do

        row <- L.index xs y

        p <- L.index row x

        let n = x + one
            l = score p $ L.reverse $ L.take x row
            r = score p             $ L.drop n row

        col <- L.index ys x

        let m = y + one
            u = score p $ L.reverse $ L.take y col
            d = score p             $ L.drop m col

        pure $ l -- left
             * r -- right
             * u -- up
             * d -- down





score :: Int -> List Int -> Int
score m = go zero
  where

    go acc Nil = acc
    go acc (x:xs) = if x >= m
        then     acc + one
        else go (acc + one) xs





collect :: List (List Int) -> Set (Int /\ Int)
collect xs = flip execState mempty do

    forWithIndex_ xs \y row ->
        forWithIndex_ (mark row) \x b ->
            when b $ modify_ $ insert $ x /\ y


    forWithIndex_ (L.transpose xs) \x row ->
        forWithIndex_ (mark row) \y b ->
            when b $ modify_ $ insert $ x /\ y





mark :: List Int -> List Boolean
mark = pair >>> uncurry (L.zipWith (||))
  where

    n1 = negate one
    init = n1 /\ n1

    check scan = L.zipWith eq <*> (map snd <<< scan init)

    pair = (check $ scanl        go)
       &&& (check $ scanr $ flip go)

    go (m /\ _) b = if m == b then (m /\ n1) else (n /\ n)
      where n = m `max` b





main :: Effect Unit
main = Console.logShow 42





run :: String -> Int /\ Int
run = S.trim >>> parse >>> (part_one &&& part_two)
  where

    parse = runParser combine >>> fromRight empty

    combine = flip C.sepBy P.whiteSpace $ C.many num

    num = P.anyDigit
        >>= SCU.singleton
        >>> I.fromString
        >>> fail "invalid" `maybe` pure

