module Y2020.Day02
  ( run
  ) where

import Prelude

import Data.Array ((!!))
import Data.Array as A
import Data.Int (fromString, odd)
import Data.Int.Bits (xor)
import Data.Maybe (Maybe, fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.String.CodeUnits as SU
import Data.Tuple (Tuple)
import Lib.Common (lines, words)





type Info
  = { l :: Int
    , h :: Int
    , c :: Char
    , s :: String
  }





part_1 :: Info -> Boolean
part_1 { l, h, c, s } = between l h $ count c s

  where

    count char = SU.toCharArray >>> A.filter (eq char) >>> A.length





part_2 :: Info -> Boolean
part_2 { l, h, c, s } = odd $ (at l) `xor` (at h)

  where

    eq_to_i a b = if a == b then 1 else 0

    at n
       =  SU.charAt (n - 1) s
      <#> eq_to_i c
       #  fromMaybe 0





str_to_row :: String -> Array Info
str_to_row str
   =  lines str
  <#> line_to_row
   #  A.catMaybes





line_to_row :: String -> Maybe Info
line_to_row str = do

    l <- list !! 0 >>= fromString
    h <- list !! 1 >>= fromString
    c <- list !! 2 >>= SU.charAt 0
    s <- list !! 3

    pure { l, h, c, s }

  where

    list = str # trim # words

    trim = rep "-" " " >>> rep ":" " "

    rep p r = S.replaceAll (S.Pattern p) (S.Replacement r)





run :: String -> Tuple Int Int
run = str_to_row >>> (count part_1 &&& count part_2)
  where

    count fn = A.filter fn >>> A.length

