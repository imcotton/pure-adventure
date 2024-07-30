module Y2022.Day02
  ( main
  , run
  ) where

import Prelude

import Data.Either (fromRight)
import Data.Foldable (sum)
import Data.Profunctor.Strong ((&&&))
import Data.String as S
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import StringParser (runParser)
import StringParser.CodeUnits as P
import StringParser.Combinators as C





data Move
  = Ro
  | Pa
  | Si

data XYZ
  = X
  | Y
  | Z

type Pair = Move /\ XYZ





calc :: Pair -> (Pair -> Move) -> Int
calc pair from = result + base converted
  where

    move /\ _ = pair
    converted = from pair
    result = move `against` converted

    base Ro = 1
    base Pa = 2
    base Si = 3

    against Ro Ro = 3
    against Pa Pa = 3
    against Si Si = 3

    against Ro Pa = 6
    against Pa Si = 6
    against Si Ro = 6

    against  _  _ = 0





part_one :: String -> Int
part_one = gen $ uncurry $ flip go
  where

    go X = const Ro
    go Y = const Pa
    go Z = const Si





part_two :: String -> Int
part_two = gen $ uncurry $ flip go
  where

    go X = case _ of
        Ro -> Si
        Pa -> Ro
        Si -> Pa

    go Y = identity

    go Z = case _ of
        Ro -> Pa
        Pa -> Si
        Si -> Ro





gen :: (Pair -> Move) -> String -> Int
gen fn = runParser total >>> fromRight zero
  where

    row = calc <$> read_row <@> fn

    list = C.sepBy row P.whiteSpace

    total = list <#> sum

    read_row = ado

        rps <- C.choice
          [ P.char 'A' $> Ro
          , P.char 'B' $> Pa
          , P.char 'C' $> Si
          ]

        P.skipSpaces

        xyz <- C.choice
          [ P.char 'X' $> X
          , P.char 'Y' $> Y
          , P.char 'Z' $> Z
          ]

        in rps /\ xyz




main :: Effect Unit
main = Console.logShow 42





run :: String -> Int /\ Int
run = S.trim >>> (part_one &&& part_two)

