module Y2022.Day07
  ( main
  , run
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (fromRight)
import Data.Filterable (filter)
import Data.Foldable (foldl)
import Data.List (List)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe, maybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Traversable (class Foldable, maximum, minimum, sum)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (chars_to_str, read_int)
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





data CMD
  = LS
  | CD String
  | DIR String
  | FILE Int String





part_one :: Int -> List CMD -> Int
part_one n = through >>> filter (_ < n) >>> sum





part_two :: Int -> Int -> List CMD -> Int
part_two total need = through >>> go >>> fromMaybe zero
  where

    go m = do

        used <- maximum m

        let available = total - used
            goal = need - available
            m' = m # filter (_ >= goal)

        minimum m'





through :: forall f. Foldable f => f CMD -> Map String Int
through = snd <<< flip go `foldl` (empty /\ empty)
  where

    go (CD  "/") = lmap $ const $ A.singleton "/root"
    go (CD "..") = lmap $ A.dropEnd 1
    go (CD name) = lmap $ flip A.snoc name

    go (FILE n _) = uncurry \path cache -> let
        cache' = count n path cache
      in
        path /\ cache'

    go _ = identity

    count _ [] acc = acc
    count n xs acc = let
        path = S.joinWith "/" xs
        m = add n `maybe n` M.lookup path acc
        acc' = M.insert path m acc
        xs' = A.dropEnd 1 xs
      in
        count n xs' acc'





main :: Effect Unit
main = Console.logShow 42





run :: String -> Int /\ Int
run = S.trim >>> parse >>> (part_one 100_000 &&& part_two 70_000_000 30_000_000)
  where

    parse = runParser combine >>> fromRight empty

    combine = flip C.sepBy P.whiteSpace $ C.choice
      [ ls    $> LS
      , cd   <#> CD
      , dir  <#> DIR
      , file <#> uncurry FILE
      ]

    ls = P.string "$ ls"

    cd = cmd <#> chars_to_str
      where cmd = P.string "$ cd " *> C.many path
            path = P.anyLetter <|> P.char '/' <|> P.char '.'

    dir = P.string "dir " *> C.many P.anyLetter <#> chars_to_str

    file = Tuple
        <$> (read_int <* P.whiteSpace)
        <*> (chars_to_str <$> C.many (P.anyLetter <|> P.char '.'))

