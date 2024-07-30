module Y2021.Day02
  ( main
  , run
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Either (hush)
import Data.Foldable (class Foldable)
import Data.Foldable as FD
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines, read_int)
import StringParser (Parser, runParser)
import StringParser.CodeUnits as P





type Point = Int /\ Int





part_one :: forall f. Foldable f => f Point -> Int
part_one = uncurry (*) <<< FD.foldl (+) (zero /\ zero)





part_two :: forall f. Foldable f => f Point -> Int
part_two = (_.x * _.y) <<< FD.foldl go { x: zero, y: zero, m: zero }
  where

    go r@{       m } (0 /\ n) = r { m = m + n }

    go r@{ x, y, m } (n /\ _) = r { x = x + n
                                  , y = y + n * m
                                  }





data Coordinate = H Boolean | V Boolean

mk_cmd :: Parser Point
mk_cmd = do

    c  <- P.string "backward" $> H false
      <|> P.string  "forward" $> H true
      <|> P.string     "down" $> V true
      <|> P.string       "up" $> V false

    P.skipSpaces

    n <- read_int

    pure case c of
        H true  ->  n /\ zero
        H false -> -n /\ zero
        V true  -> zero /\  n
        V false -> zero /\ -n





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> Int /\ Int
run = S.trim
    >>> lines
    >>> A.mapMaybe (runParser mk_cmd >>> hush)
    >>> (part_one &&& part_two)

