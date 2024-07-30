module Y2020.Day09
  ( main
  , run
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((!!))
import Data.Array as A
import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.Foldable as F
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((***))
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)





part_1 :: Int -> Array BigInt -> BigInt
part_1 size array = go 0
  where

    go i = let
        chunk = array # A.slice i (i + size + 1)
        done = size + 1 /= A.length chunk

      in if done then zero else case A.unsnoc chunk of
        Nothing -> zero
        Just { init, last } -> if valid last init
            then go (i + 1)
            else last

    valid n arr = let each = flip A.any arr
      in each \a -> let x = n - a
      in each \b -> b == x && a /= x





part_2 :: BigInt -> Array BigInt -> BigInt
part_2 num array = fromMaybe zero $ compute =<< tailRecM go { i: 0, j: 1 }
  where

    compute = lift2 (+) <$> F.minimum <*> F.maximum

    cusum = A.scanl (+) zero array

    go r@{ i, j } = do
        a <- cusum !! i
        b <- cusum !! j
        case compare (b - a) num of
            GT -> pure $ Loop r { i = i + 1 }
            LT -> pure $ Loop r { j = j + 1 }
            EQ -> pure $ Done $ array # A.slice i j





main :: Effect Unit
main = do
    Console.logShow 42





run :: Int -> String -> Tuple String String
run num = S.trim >>> lines >>> A.mapMaybe BI.fromString >>> combine
  where

    to_str = BI.toString >>> (_ <> "n")

    combine arr = to_str *** to_str $ Tuple one two
      where
        one = part_1 num arr
        two = part_2 one arr

