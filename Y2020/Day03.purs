module Y2020.Day03
  ( run
  ) where

import Prelude

import Data.Array ((!!))
import Data.BigInt as BI
import Data.Foldable as F
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple)
import Lib.Common (lines)





loop :: Int -> Int -> Array String -> Int
loop sx sy arr = go sx sy 0

  where

    count '#' = 1
    count   _ = 0

    go x y acc = case arr !! y of

        Nothing -> acc

        Just row -> case count <$> SCU.charAt x row of

            Nothing -> go (x `mod` SCU.length row) y acc

            Just n -> go (x + sx) (y + sy) (acc + n)





part_1 :: Array String -> Int
part_1 = loop 3 1

part_2 :: Array String -> String
part_2 = ado

    a <- loop 1 1
    b <- loop 3 1
    c <- loop 5 1
    d <- loop 7 1
    e <- loop 1 2

    in compute [ a, b, c, d, e ]

  where

    compute
       = map BI.fromInt
     >>> F.product
     >>> BI.toString
     >>> (_ <> "n")





run :: String -> Tuple Int String
run = S.trim >>> lines >>> (part_1 &&& part_2)

