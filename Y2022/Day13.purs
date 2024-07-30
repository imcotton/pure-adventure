module Y2022.Day13
  ( main
  , run
  ) where

import Prelude

import Control.Lazy (defer)
import Data.Either (fromRight)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.List (List(..), elemIndex, sort, (:))
import Data.Maybe (fromMaybe)
import Data.Profunctor.Strong ((&&&), (***))
import Data.String as S
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (read_int)
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





data Packet
  = Num Int
  | Arr (List Packet)

derive instance Eq Packet

instance Ord Packet where
    compare a@(Num _ ) b@(Arr _ ) = compare   (Arr $ pure a) b
    compare a@(Arr _ ) b@(Num _ ) = compare a (Arr $ pure b)
    compare   (Num x )   (Num y ) = compare x  y
    compare   (Arr xs)   (Arr ys) = compare xs ys





part_one :: forall f a. Ord a => FoldableWithIndex Int f => f (a /\ a) -> Int
part_one = foldlWithIndex go zero
  where

    go i acc (a /\ b) = if a < b then acc + i + one
                                 else acc





part_two :: List (Packet /\ Packet) -> Int
part_two xs = (xs >>= flat) <> (mk2 : mk6 : Nil)
            # sort
            # (elemIndex mk2 &&& elemIndex mk6)
            # (plus *** plus)
            # uncurry (*)
            # fromMaybe zero

  where

    plus = add one

    flat (a /\ b) = a : b : Nil

    mk2 = mk_ 2
    mk6 = mk_ 6
    mk_ = Num >>> pure >>> Arr
              >>> pure >>> Arr





main :: Effect Unit
main = Console.logShow 42





run :: String -> (Int /\ Int)
run = S.trim >>> parse >>> (part_one &&& part_two)
  where

    parse = runParser (pair `C.sepBy` P.whiteSpace) >>> fromRight mempty

    pair = Tuple <$> packet <* P.whiteSpace
                 <*> packet

    packet = defer \_ -> C.choice
      [ Num <$> read_int
      , Arr <$> read_arr
      ]

    read_arr = defer \_ -> within $ packet `C.sepBy` P.char ','

    within = C.between (P.char '[')
                       (P.char ']')

