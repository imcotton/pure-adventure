module Y2022.Day04
  ( main
  , run
  ) where

import Prelude

import Control.Plus (empty)
import Data.Array.NonEmpty as A
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (List)
import Data.List as L
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set as Se
import Data.String as S
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (read_int)
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





type Range = Int /\ Int





part_one :: Set Int -> Set Int -> Boolean
part_one a b = Se.subset a b
            || Se.subset b a





part_two :: Set Int -> Set Int -> Boolean
part_two a b = not Se.isEmpty $ Se.intersection a b





gen :: (Set Int -> Set Int -> Boolean) -> List (Range /\ Range) -> Int
gen fn = let
    fn' = uncurry $ fn `on` r2s
    r2s = uncurry A.range >>> Se.fromFoldable
  in
    L.filter fn' >>> L.length





main :: Effect Unit
main = Console.logShow 42





run :: String -> Int /\ Int
run = S.trim >>> parse >>> (gen part_one &&& gen part_two)
  where

    range = Tuple
        <$> read_int <* P.char '-'
        <*> read_int

    pair = Tuple
        <$> range <* P.char ','
        <*> range

    line = C.sepBy pair P.whiteSpace

    parse = runParser line >>> fromRight empty

