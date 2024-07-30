module Y2022.Day09
  ( main
  , run
  ) where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Either (fromRight)
import Data.Foldable (class Foldable, foldMap, length)
import Data.Function (applyN)
import Data.Generic.Rep (class Generic)
import Data.Int (pow, toNumber)
import Data.Number (sqrt)
import Data.Profunctor.Strong ((&&&))
import Data.Semigroup.Foldable (minimumBy)
import Data.Show.Generic (genericShow)
import Data.String as S
import Data.Traversable (class Traversable, scanl)
import Data.Tuple (Tuple(..), swap, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (read_int)
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





data Direct
  = Up
  | Dn
  | Rt
  | Lf

derive instance Generic Direct _
instance Show Direct where show = genericShow





type Point = Int /\ Int





part_by :: forall t. Foldable t => Int -> t (Direct /\ Int) -> Int
part_by n = trail >>> applyN follow n >>> A.nub >>> length





trail :: forall f. Foldable f => f (Direct /\ Int) -> Array Point
trail = foldMap expand >>> scanl go (zero /\ zero)
  where

    expand = swap >>> uncurry replicate

    go (x /\ y) = case _ of
        Up -> x /\ (y + one)
        Dn -> x /\ (y - one)
        Rt -> (x + one) /\ y
        Lf -> (x - one) /\ y





follow :: forall t. Traversable t => t Point -> t Point
follow = scanl go (zero /\ zero)
  where

    go from to = if next == to then from else next
      where next = closest from to

    -- | * * *
    -- | * f ▶ t
    -- | * * *   ◤ * *
    -- |         * f *
    -- |         * * *
    closest from to = minimumBy comp $ around from
      where comp = comparing $ distance to

    -- | * * *
    -- | * p *
    -- | * * *
    around p = add <$> ps <@> p
      where ps = Tuple <$> ns <*> ns
            ns = NA.cons' zero [ one, negate one ]

    distance (xa /\ ya) (xb /\ yb) = sqrt z
      where square = flip pow 2
            x = square $ xa - xb
            y = square $ ya - yb
            z = x + y # toNumber





main :: Effect Unit
main = Console.logShow 42





run :: String -> (Int /\ Int)
run = S.trim >>> parse >>> (part_by 1 &&& part_by 9)
  where

    parse = runParser line >>> fromRight mempty

    line = C.sepBy direct P.whiteSpace

    direct = Tuple <$> u_d_l_r <* P.whiteSpace
                   <*> read_int

    u_d_l_r = C.choice
      [ P.char 'U' $> Up
      , P.char 'D' $> Dn
      , P.char 'R' $> Rt
      , P.char 'L' $> Lf
      ]

