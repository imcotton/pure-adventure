module Y2022.Day05
  ( main
  , run
  ) where

import Prelude

import Control.Plus (empty, (<|>))
import Data.Array as A
import Data.Either (fromRight)
import Data.Foldable (foldM)
import Data.List (List)
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (read_int, split_by)
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





data Operation = Operation
    Int  -- move n
    Int  -- from i
    Int  --   to j

type Stacks = List (List Char)

type Actions = List Operation





part_by :: (List ~> List) -> (Stacks /\ Actions) -> String
part_by fn (stack /\ actions) = fromMaybe mempty fa
  where

    fa = result >>= traverse L.last
                <#> (A.fromFoldable >>> SCU.fromCharArray)

    result = stack `looping` actions

    looping = foldM \acc (Operation n i j) -> do

        peak <- fn <<< L.takeEnd n <$> L.index acc i

        acc' <- L.modifyAt i (L.dropEnd n) acc

        L.modifyAt j (_ <> peak) acc'





main :: Effect Unit
main = Console.logShow 42





run :: String -> String /\ String
run = S.trim >>> split_by "\n\n"
             >>> parse
             >>> (part_by L.reverse &&& part_by identity)
  where

    top = map trim <$> L.transpose <$> C.sepBy fab P.whiteSpace
      where
        a = C.between (P.char '[') (P.char ']') $ P.anyLetter
        b = C.between (P.char ' ') (P.char ' ') $ P.char ' '
        a_b = a <|> b
        fab = C.sepBy a_b $ P.char ' '
        trim = L.reverse >>> L.takeWhile (notEq ' ')

    read_index = read_int <#> (_ - one)

    bottom = flip C.sepBy P.whiteSpace $ Operation
        <$> (P.string "move " *> read_int   <* P.whiteSpace)
        <*> (P.string "from " *> read_index <* P.whiteSpace)
        <*> (P.string   "to " *> read_index)

    parse (before /\ after) = fromRight (empty /\ empty) $ Tuple
        <$> runParser top before
        <*> runParser bottom after

