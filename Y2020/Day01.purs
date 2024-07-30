module Y2020.Day01
  ( run
  ) where

import Prelude

import Control.MonadZero (guard)
import Data.Array as A
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Lib.Common as Comm






of_2 :: Int -> Array Int -> Array Int
of_2 _ [] = []
of_2 n xs = do
    x <- xs
    y <- xs
    guard $ x + y == n
    pure  $ x * y

of_3 :: Int -> Array Int -> Array Int
of_3 _ [] = []
of_3 n xs = do
    x <- xs
    y <- xs
    z <- xs
    guard $ x + y + z == n
    pure  $ x * y * z




run :: Int -> String -> Tuple Int Int
run n raw = Tuple

    (trim $ of_2 n list)
    (trim $ of_3 n list)

  where

    list = raw # Comm.str_to_arr_int
    trim = A.head >>> fromMaybe 0

