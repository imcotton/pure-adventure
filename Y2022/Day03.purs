module Y2022.Day03
  ( main
  , run
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind (bindFlipped)
import Control.Plus (empty)
import Data.Char (toCharCode)
import Data.Either (fromRight)
import Data.Foldable (sum)
import Data.List (List)
import Data.List as L
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





part_one :: List (List Int) -> Int
part_one = sum <<< bindFlipped do
    m <- (_ / 2) <<< L.length
    l <- L.take m
    r <- L.takeEnd m
    pure $ L.nub $ L.intersect l r





part_two :: Int -> List (List Int) -> Int
part_two n = go zero
  where

    go acc L.Nil = acc
    go acc xs = let
        init = L.take n xs
        tail = L.drop n xs
        acc' = sum $ L.nub $ L.foldl fn empty init
      in
        go (acc + acc') tail

    fn L.Nil xs = xs
    fn l r = L.intersect l r





main :: Effect Unit
main = Console.logShow 42





run :: String -> Int /\ Int
run = S.trim >>> parse >>> (part_one &&& part_two 3)
  where

    l = P.lowerCaseChar <#> toCharCode <#> (_ - 96)
    u = P.upperCaseChar <#> toCharCode <#> (_ - 38)
    s = C.many $ l <|> u
    z = C.sepBy s P.whiteSpace
    parse = runParser z >>> fromRight empty
