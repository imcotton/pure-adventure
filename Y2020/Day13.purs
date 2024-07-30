module Y2020.Day13
  ( main
  , run
  ) where

import Prelude

import Control.Lazy (fix)
import Control.Monad.State as ST
import Data.Array as A
import Data.BigInt as BI
import Data.Foldable (minimum)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)





part_1 :: Int /\ Array Int -> Int
part_1 = uncurry \n -> A.filter (_ > 0)
    >>> map ((\m -> m - (n `mod` m)) &&& identity)
    >>> minimum
    >>> map (uncurry mul)
    >>> fromMaybe 0





part_2 :: Int /\ Array Int -> String
part_2 = uncurry $ const $ map BI.fromInt
    >>> traverseWithIndex_ go
    >>> flip ST.execState { t: zero, s: zero }
    >>> _.t
    >>> BI.toString
    >>> (_ <> "n")

  where

    go 0 b = ST.modify_ \r -> r { s = b }
    go _ b | b == zero = mempty
    go n b =
      let
        i = BI.fromInt n
      in
        ST.modify_ \{ t, s } -> t # fix \loop j ->
            if (j + i) `mod` b == zero
            then { t: j, s: s * b }
            else loop $ j + s





mk_input :: Array String -> Maybe (Int /\ Array Int)
mk_input arr = let arr' = S.trim <$> arr in ado

    time <- A.head arr' >>= fromString

    list <- A.last arr'
        <#> S.replaceAll (S.Pattern "x") (S.Replacement "0")
        >>> S.split (S.Pattern ",")
        >>> A.mapMaybe fromString

    in time /\ list





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> Int /\ String
run = S.trim >>> lines >>> mk_input >>> map (part_1 &&& part_2)
    >>> fromMaybe (0 /\ "0n")

