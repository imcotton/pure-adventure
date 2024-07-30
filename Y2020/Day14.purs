module Y2020.Day14
  ( main
  , run
  ) where

import Prelude

import Control.Monad.State (State)
import Control.Monad.State as ST
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.Foldable as FD
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Data.Profunctor.Strong ((&&&), (***))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Traversable as T
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)





data Command = Mask String | Var BigInt BigInt

type Mem = Map BigInt BigInt

type Cache
  = { mask :: Array Char
    , memo :: Mem
    }

init :: Cache
init = { mask: [], memo: M.empty }





part_1 :: Array Command -> State Cache (Array Unit)
part_1 = T.traverse $ reading calc
  where

    to '1' _ = '1'
    to '0' _ = '0'
    to  _  n =  n

    calc k v = do

        mask <- ST.gets _.mask

        let value  = SCU.toCharArray $ to_binary v $ FD.length mask
            value' = A.zipWith to mask value
                   # SCU.fromCharArray
                   # BI.fromBase 2
                   # fromMaybe v

        ST.modify_ \r -> r { memo = M.insert k value' r.memo }





part_2 :: Array Command -> State Cache (Array Unit)
part_2 = T.traverse $ reading calc
  where

    to '1' _ = '1'
    to 'X' _ = 'X'
    to  _  n =  n

    calc k v = do

        mask <- ST.gets _.mask

        let value = SCU.toCharArray $ to_binary k $ FD.length mask

        FD.for_ (unmask $ A.zipWith to mask value) \n ->

            ST.modify_ \r -> r { memo = M.insert n v r.memo }

    table '0' = [ zero      ]
    table '1' = [ one       ]
    table 'X' = [ zero, one ]
    table  _  = [           ]

    unmask = FD.foldl go [ zero ]
      where
        two = one + one
        carry = (+) <<< flip mul two
        go acc x = carry <$> acc <*> table x





reading
    :: (BigInt -> BigInt -> State Cache Unit)
    -> Command
    -> State Cache Unit
reading var = case _ of
    Mask m -> ST.modify_ $ _ { mask = SCU.toCharArray m }
    Var k v -> var k v





to_binary :: BigInt -> Int -> String
to_binary n p =
  let
    m = BI.toBase (one + one) n
  in pad_start m p '0'





pad_start :: String -> Int -> Char -> String
pad_start s n c =
  let
    l = SCU.length s
    z = flip A.replicate c $ (l `max` n) - l
  in SCU.fromCharArray z <> s





rep :: String -> String -> String -> String
rep = curry $ (S.Pattern *** S.Replacement) >>> uncurry S.replaceAll





mk_command :: String -> Maybe Command
mk_command str = do

    let pair = str # S.trim
            >>> rep "mem[" ""
            >>> rep    "]" ""
            >>> S.split (S.Pattern " = ")

    left <- A.head pair
    right <- A.last pair

    if left == "mask"
    then pure $ Mask right
    else ado

        k <- BI.fromString left
        v <- BI.fromString right

        in Var k v





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> String /\ String
run = S.trim
    >>> lines
    >>> A.mapMaybe mk_command
    >>> (part_1 &&& part_2)
    >>> bimap exec exec

  where

    exec = flip ST.execState init
        >>> _.memo
        >>> FD.sum
        >>> BI.toString
        >>> (_ <> "n")

