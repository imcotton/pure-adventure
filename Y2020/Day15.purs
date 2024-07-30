module Y2020.Day15
  ( main
  , run
  ) where

import Prelude

import Data.Array as A
import Data.FoldableWithIndex as FDI
import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Int (fromString)
import Data.Maybe (fromMaybe, maybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class.Console as Console





part_1 :: Array Int -> Int -> Int
part_1 input m = go index last cache
  where

    go i n c = if i >= m then n else
      let
        i' = i + one
        n' = M.lookup n c # maybe zero (i - _)
        c' = M.insert n i c
      in
        go i' n' c'

    index = input # A.length
    last  = input # A.last # fromMaybe zero
    cache = input # A.dropEnd 1 # mk_cache




part_2 :: Array Int -> Int -> Int
part_2 = part_1





mk_cache :: Array Int -> HashMap Int Int
mk_cache = flip FDI.foldlWithIndex M.empty \i m v -> M.insert v (i + one) m





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> (Int -> Int) /\ (Int -> Int)
run = S.trim >>> S.split (S.Pattern ",") >>> A.mapMaybe fromString >>> (part_1 &&& part_2)

