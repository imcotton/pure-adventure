module Y2021.Day04
  ( main
  , run
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State (class MonadState, evalStateT, modify)
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Either (fromLeft)
import Data.Filterable (filter)
import Data.Foldable as FD
import Data.Functor.Compose (Compose(..))
import Data.Int (fromString)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines, words)
import Matrix (Matrix)
import Matrix as M





type Grid = Matrix Int





part_one :: forall f. TraversableWithIndex Int f => f Int -> f Grid -> Int
part_one x xs = part x xs # FD.minimum # zero `maybe` snd

part_two :: forall f. TraversableWithIndex Int f => f Int -> f Grid -> Int
part_two x xs = part x xs # FD.maximum # zero `maybe` snd





part :: forall f.
    TraversableWithIndex Int f =>
        f Int ->
        f Grid ->
        f (Int /\ Int)
part x xs = let
    go = evalStateT <<< traverseWithIndex examine
  in
    xs <#> go x <#> fromLeft (zero /\ zero)





examine :: forall m.
    Monad m =>
    MonadState Grid m =>
    MonadThrow (Int /\ Int) m =>
        Int ->
        Int ->
        m Unit
examine i j = do

    marked <- modify $ mark j

    let prompt = M.toIndexedArray
            >>> map _.value
            >>> filter (_ > zero)
            >>> FD.sum
            >>> (*) j
            >>> Tuple i
            >>> throwError

    check marked `when` prompt marked

  where

    mark n = map \m -> if m == n then -m else m

    check = let
        through = FD.any $ FD.all (_ < zero)
      in
        through <<< M.rows || through <<< M.columns





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> Int /\ Int
run raw = raw
    #  S.trim
    #  S.split (S.Pattern "\n\n")
   <#> S.trim
    #  (NA.fromFoldable >>> map NA.toNonEmpty)

   <#> (\(x :| xs) -> Tuple

        (x  # S.split (S.Pattern ",")
            # A.mapMaybe fromString
        )

        (xs #  A.fromFoldable
           <#> lines
            #  Compose
           <#> S.trim
           >>> words
           >>> A.mapMaybe fromString
            #  unwrap
            #  A.mapMaybe M.fromArray
        )

    )

    #  maybe (0 /\ 0) (uncurry part_one &&& uncurry part_two)

