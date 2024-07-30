module Y2021.Day08
  ( main
  , run
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Array as A
import Data.Filterable (class Filterable)
import Data.Filterable as FT
import Data.Foldable (class Foldable)
import Data.Foldable as FD
import Data.HeytingAlgebra (ff)
import Data.Int as I
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)





part_one :: forall f.
    Foldable f =>
    Filterable f =>
        f (f String /\ f String) ->
        Int
part_one arr = arr
    <#> snd
    <#> map S.length
    <#> find [ 2, 4, 3, 7 ]
    <#> FD.length
     #  FD.sum

  where

    find = FT.filter <<< FD.foldl (||) ff <<< map (==)


{-
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....


 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
-}



table :: Array String
table =
  [ "1110111"
  , "0011000"
  , "1011101"
  , "1011011"
  , "0111010"
  , "1101011"
  , "1101111"
  , "1010010"
  , "1111111"
  , "1111011"
  ]

table_num :: Array Int
table_num = let
    map_bin = A.mapMaybe $ I.fromStringAs I.binary
  in
    map_bin table


ex :: String
ex = """
bcedagf ebadf gcdfe gfcead bcedgf dfeca ac dgca ace cafbge | ecfdbag gecfd feadb degacbf
gab cebfag bdfg gaefd bg debga gadbef dbafgce gdafec cadbe | befdga fgdaec gdaecf bdecgaf
"""


main :: Effect Unit
main = do
    traceM $ A.zipWith Tuple table table_num
    Console.logShow 42


run :: String -> Tuple Int Int
run = S.trim
   >>> lines
   >>>      map (S.split (S.Pattern "|"))
   >>> map (map (S.trim >>> S.split (S.Pattern " ")))
   >>> A.mapMaybe (lift2 Tuple <$> A.head <*> A.last)
   >>> (part_one &&& part_one)

