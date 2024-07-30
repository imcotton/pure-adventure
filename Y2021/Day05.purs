module Y2021.Day05
  ( main
  , run
  ) where

import Prelude

import Control.Plus (empty)
import Data.Array ((..))
import Data.Array as A
import Data.Either (hush)
import Data.Foldable as FD
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines, read_int)
import StringParser (Parser, runParser)
import StringParser.CodeUnits as P





type Pos = Int /\ Int
type Line = Pos /\ Pos





part_one :: Array Line -> Int
part_one = count $ uncurry $ draw false

part_two :: Array Line -> Int
part_two = count $ uncurry $ draw true





count :: (Line -> Array Pos) -> Array Line -> Int
count f = map f
    >>> A.concat
    >>> A.groupAll
    >>> map FD.length
    >>> A.filter (_ > 1)
    >>> A.length





draw :: Boolean -> Pos -> Pos -> Array Pos
draw cross (x1 /\ y1) (x2 /\ y2)

  | x1 == x2 = [      Tuple x1 ] <*> (y1 .. y2)

  | y1 == y2 = [ flip Tuple y1 ] <*> (x1 .. x2)

  | cross, x1 + y1 == x2 + y2
        || x1 + y2 == x2 + y1 = A.zipWith Tuple (x1 .. x2) (y1 .. y2)

  | otherwise = empty





mk_line :: Parser Line
mk_line = do

    p1 <- read_pos
    _  <- P.string " -> "
    p2 <- read_pos

    pure $ p1 /\ p2

  where

    read_pos = do

        x <- read_int
        _ <- P.char ','
        y <- read_int

        pure $ x /\ y





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> Int /\ Int
run = S.trim
   >>> lines
   >>> A.mapMaybe (runParser mk_line >>> hush)
   >>> (part_one &&& part_two)

