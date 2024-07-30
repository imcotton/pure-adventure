module Y2020.Day05
  ( run
  ) where

import Prelude

import Data.Array as A
import Data.Foldable as F
import Data.Maybe (fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple)
import Lib.Common (lines)





calc :: Char -> Char -> String -> Int
calc lo hi = F.foldl go 0 <<< SCU.toCharArray
  where
    go acc c = n
      where
        t = 2 * acc
        n | c == lo = t
          | c == hi = t + 1
          | otherwise = 0





seat :: String -> Int
seat str = row * 8 + col
  where
    row = calc 'F' 'B' $ SCU.take 7 str
    col = calc 'L' 'R' $ SCU.drop 7 str





part_1 :: Array String -> Int
part_1 = F.foldl go 0
  where
    go acc = max acc <<< seat





part_2 :: Array String -> Int
part_2 arr = fromMaybe 0 do

    let seats = arr <#> seat

    total <- A.range <$> F.minimum seats <*> F.maximum seats

    pure $ (F.sum total) - (F.sum seats)





run :: String -> Tuple Int Int
run = S.trim >>> lines >>> (part_1 &&& part_2)

