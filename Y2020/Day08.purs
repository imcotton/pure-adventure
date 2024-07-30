module Y2020.Day08
  ( main
  , run
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Array as A
import Data.Either (hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.Set as Set
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Lib.Common (lines)
import Text.Parsing.StringParser (fail, runParser)
import Text.Parsing.StringParser.CodeUnits as P





type Sheet = Array Commands

data Commands
  = NOP Int
  | JMP Int
  | ACC Int
  | END Int

instance showCommands :: Show Commands where
   show (NOP n) = "(NOP " <> show n <> ")"
   show (JMP n) = "(JMP " <> show n <> ")"
   show (ACC n) = "(ACC " <> show n <> ")"
   show (END n) = "(END " <> show n <> ")"





part_1 :: Sheet -> Int
part_1 sheet = go 0 0 Set.empty
  where

    end = END 0

    go acc i set =

      let seen = Set.member i set
          set' = Set.insert i set
          next = fromMaybe end $ sheet !! i

      in if seen then acc else case next of

        END _ ->     acc
        NOP _ -> go  acc      (i + 1) set'
        JMP n -> go  acc      (i + n) set'
        ACC n -> go (acc + n) (i + 1) set'





part_2 :: Sheet -> Int
part_2 sheet = go 0 0 0 Set.empty sheet
  where

    end = END 0

    toggle i arr = do
        j <- arr # A.drop i # A.findIndex mark
        let j' = j + i + 1
        new <- arr # A.modifyAt (j' - 1) switch
        pure $ Tuple j' new

    mark x | JMP _ <- x = true
           | NOP _ <- x = true
           | otherwise  = false

    switch x | JMP n <- x = NOP n
             | NOP n <- x = JMP n
             | otherwise  = x

    go acc i j set arr =

      let seen = Set.member i set
          set' = Set.insert i set
          next = fromMaybe end $ arr !! i

      in if seen then case toggle j sheet of

        Nothing              -> acc
        Just (Tuple j' arr') -> go 0 0 j' Set.empty arr'

      else case next of

        END _ ->     acc
        NOP _ -> go  acc      (i + 1) j set' arr
        JMP n -> go  acc      (i + n) j set' arr
        ACC n -> go (acc + n) (i + 1) j set' arr





mk_sheet :: String -> Sheet
mk_sheet = lines >>> A.mapMaybe (S.trim >>> parse_line >>> hush)

  where

    parse_line = runParser do

        cmd <- P.string "nop" $> NOP
           <|> P.string "jmp" $> JMP
           <|> P.string "acc" $> ACC

        P.skipSpaces

        sign <- P.char '+' $> identity
            <|> P.char '-' $> negate

        num <- P.regex "[0-9]+" >>= \str -> case fromString str of
            Just n -> pure n
            Nothing -> fail $ "wrong Int " <> str

        pure $ cmd $ sign num





main :: Effect Unit
main = do
    Console.logShow 42





run :: String -> Tuple Int Int
run = mk_sheet >>> (part_1 &&& part_2)

