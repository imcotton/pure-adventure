module Y2020.Day11
  ( main
  , run
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (execState, modify_)
import Data.Array as A
import Data.Filterable (class Filterable)
import Data.Filterable as FT
import Data.Foldable (class Foldable)
import Data.Foldable as FD
import Data.FoldableWithIndex as FDI
import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)





type Grid = HashMap Point Cell

type Point = { x :: Int, y :: Int }

data Cell = EMPTY | TAKEN | FLOOR

derive instance Eq Cell

instance Show Cell where
    show EMPTY = "L"
    show TAKEN = "#"
    show FLOOR = "."





pos :: Array Point
pos = { x: _, y: _ }
   <$> [ -1, 0, 1 ]
   <*> [ -1, 0, 1 ]
    #  FT.filter (_ /= { x: 0, y: 0 })





part_1 :: Grid -> Int
part_1 = grind (through 4 neighbors) >>> count_grid
  where

    neighbors :: Point -> Grid -> Int
    neighbors base = sequence queries >>> A.catMaybes >>> count
      where

        queries = pos <#> add base >>> M.lookup





part_2 :: Grid -> Int
part_2 = grind (through 5 neighbors) >>> count_grid
  where

    neighbors :: Point -> Grid -> Int
    neighbors base = sequence queries >>> A.catMaybes >>> count
      where

        queries = pos <#> crawl base

    crawl :: Point -> Point -> Grid -> Maybe Cell
    crawl base step cache = flip tailRecM base $ go <<< add step
      where

        go next = M.lookup next cache <#> \cell ->
            if cell == FLOOR
            then Loop next
            else Done cell





through :: Int -> (Point -> Grid -> Int) -> Grid -> Grid
through n search origin = FDI.foldlWithIndex go origin seats
  where

    seats = M.filter (_ /= FLOOR) origin

    with = flip search origin

    go key cache cell =
       let cell' = switch n cell $ with key in
        if cell' /= cell
        then cache # M.insert key cell'
        else cache





switch :: Int -> Cell -> Int -> Cell
switch n seat m = go
  where

    go | seat == EMPTY, m <= 0 = TAKEN
       | seat == TAKEN, m >= n = EMPTY
       |             otherwise = seat





grind :: forall s. Eq s => (s -> s) -> s -> s
grind f = go <*> f
  where

    go  old    new
      | old == new =    new
      |  otherwise = go new $ f new





count :: forall f. Filterable f => Foldable f => f Cell -> Int
count = FT.filter (eq TAKEN) >>> FD.length

count_grid :: Grid -> Int
count_grid = M.filter (_ == TAKEN) >>> FD.length





mk_row :: String -> Array Cell
mk_row = S.trim
    >>> S.split (S.Pattern "")
    >>> A.mapMaybe case _ of
        "L" -> Just EMPTY
        "#" -> Just TAKEN
        "." -> Just FLOOR
        _   -> Nothing





mk_grid :: Array (Array Cell) -> Grid
mk_grid arr = flip execState M.empty do

    FDI.forWithIndex_ arr \y row ->

        FDI.forWithIndex_ row \x cell ->

            modify_ $ M.insert { x, y } cell





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> Tuple Int Int
run = S.trim >>> lines >>> map mk_row >>> mk_grid >>> (part_1 &&& part_2)

