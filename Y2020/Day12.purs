module Y2020.Day12
  ( main
  , run
  ) where

import Prelude

import Control.Plus (empty)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Data.Ord (abs)
import Data.Profunctor.Strong ((&&&))
import Data.Show.Generic (genericShow)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Traversable as F
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)





data Nav

  = NN Int
  | SS Int
  | EE Int
  | WW Int

  | LL Int
  | RR Int

  | FF Int

derive instance Generic Nav _

instance Show Nav where
  show = genericShow





part_1 :: Array Nav -> Int
part_1 = F.foldl d2p { x: 0, y: 0, c: 0 }
    >>> (_.x >>> abs + abs <<< _.y)

  where

    d2p p@{ x, y, c } = case _ of

        NN n -> p { y = y + n }
        SS n -> p { y = y - n }
        EE n -> p { x = x + n }
        WW n -> p { x = x - n }

        LL n -> p { c =  c + n            }
        RR n -> p { c = (c - n) `mod` 360 }

        FF n -> d2p p $ c2d c n

    c2d c | c < 90 * 1 = EE
          | c < 90 * 2 = NN
          | c < 90 * 3 = WW
          | c < 90 * 4 = SS
          | otherwise = c2d $ c `mod` 360





part_2 :: Array Nav -> Int
part_2 = F.foldl d2p { x: 0, y: 0, wx: 10, wy: 1 }
    >>> (_.x >>> abs + abs <<< _.y)

  where

    d2p p@{ x, y, wx, wy } = case _ of

        NN n -> p { wy = wy + n }
        SS n -> p { wy = wy - n }
        EE n -> p { wx = wx + n }
        WW n -> p { wx = wx - n }

        LL n -> rotate $ -n
        RR n -> rotate $  n

        FF n -> p { x = x + wx * n
                  , y = y + wy * n
                  }

      where

        rotate n = case (n / 90) `mod` 4 of

            1 -> p { wx =  wy, wy = -wx }
            2 -> p { wx = -wx, wy = -wy }
            3 -> p { wx = -wy, wy =  wx }
            _ -> p





mk_cmd :: String -> Maybe Nav
mk_cmd = S.trim >>> SCU.uncons >=> \{ head, tail } -> ado

    n <- fromString tail

    c <- case head of

        'N' -> pure NN
        'S' -> pure SS
        'E' -> pure EE
        'W' -> pure WW

        'L' -> pure LL
        'R' -> pure RR

        'F' -> pure FF
        _   -> empty

    in c n





main :: Effect Unit
main = do
    Console.logShow 24





run :: String -> Tuple Int Int
run = S.trim >>> lines >>> A.mapMaybe mk_cmd >>> (part_1 &&& part_2)

