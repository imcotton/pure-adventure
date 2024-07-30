module Y2022.Day12
  ( main
  , run
  ) where

import Prelude

import Control.Alternative (guard, (<|>))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as A
import Data.Either (fromRight)
import Data.Enum (succ)
import Data.Filterable (filter)
import Data.Foldable (length, minimumBy)
import Data.FoldableWithIndex (findMapWithIndex)
import Data.Lens (preview, (^?))
import Data.Lens.Index (ix)
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Ord (abs)
import Data.Profunctor.Strong ((&&&), (***))
import Data.String as S
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (unfoldr)
import Effect (Effect)
import Effect.Class.Console as Console
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





type Point = Int /\ Int
type Pair a = Point /\ a
type Grid a = Array (Array a)





uphill :: Char -> Char -> Boolean
uphill 'S' 'a' = true
uphill 'S'  _  = false
uphill 'z' 'E' = true
uphill  _  'E' = false
uphill  a   b  = succ a >= pure b





part_one :: Grid Char -> Array Point
part_one gd = maybe mempty (gen eq heuristic expand) pair
  where

    expand = spread uphill gd

    pair = Tuple <$> lookup 'S' gd
                 <*> lookup 'E' gd

    -- Manhattan distance for A*
    heuristic (ax /\ ay) (bx /\ by) = dx + dy
      where dx = abs $ ax - bx
            dy = abs $ ay - by





part_two :: Grid Char -> Array Point
part_two gd = maybe mempty (gen met heuristic expand) pair
  where

    expand = spread (flip uphill) gd

    pair = Tuple <$> lookup 'E' gd
                 <*> lookup 'a' gd

    -- BFS for Dijkstra
    heuristic _ _ = zero

    met p _ = find p gd == pure 'a'





find :: forall a. Point -> Grid a -> Maybe a
find (x /\ y) = preview $ ix y <<< ix x





gen ::
    (Point -> Point -> Boolean) ->
    (Point -> Point -> Int) ->
    (Point -> Array Point) ->
    Pair Point ->
    Array Point
gen met dt expand = \(start /\ end) -> fromMaybe mempty $ tailRecM (go end)
  { close: M.empty
  , open: start `M.singleton` calc zero (bottom /\ bottom) end start
  }

  where

    pop queue = highest queue >>= _.p >>> flip M.pop queue

    highest = minimumBy $ comparing \{ f, h } -> f * 0xFFFF + h

    calc g prev goal p = { g, h, f, p, prev }
      where h = dt p goal
            f = g + h

    pick old new = if old.f <= new.f then old else new

    retreat xs = unfoldr \p -> Tuple p <$> xs ^? ix p

    go goal acc = ado

        { g, p, prev } /\ old <- pop acc.open

        let hops = expand p # filter (not $ flip M.member acc.close)
            neighbors = hops <#> calc (g + one) p goal
            new = neighbors <#> (flip Tuple <*> _.p) # M.fromFoldable
            open = M.unionWith pick old new
            close = M.insert p prev acc.close

        in if p `not met` goal then Loop { open, close }
                               else Done $ A.drop 1
                                         $ A.reverse
                                         $ retreat close p





spread :: forall a. (a -> a -> Boolean) -> Grid a -> Point -> Array Point
spread ft gd pt = fromMaybe mempty ado
    v <- find pt gd
  in
    pt # around
       # A.mapMaybe (find_with gd)
       # filter (snd >>> ft v)
       # map fst

  where

    find_with g p = Tuple p <$> find p g

    around (x /\ y) =
      [ x /\ (y + one)
      , x /\ (y - one)
      ,      (x - one) /\ y
      ,      (x + one) /\ y
      ]





lookup :: forall a. Eq a => a -> Grid a -> Maybe Point
lookup a =
    findMapWithIndex \y ->
        findMapWithIndex \x b -> ado
            guard $ a == b
          in
            x /\ y





main :: Effect Unit
main = Console.logShow 42





run :: String -> (Int /\ Int)
run = S.trim >>> parse >>> (part_one &&& part_two) >>> (length *** length)
  where

    parse = runParser ys >>> fromRight mempty

    ys = A.fromFoldable <$> C.sepBy xs P.whiteSpace
    xs = A.fromFoldable <$> C.many c
    c = P.upperCaseChar <|> P.lowerCaseChar

