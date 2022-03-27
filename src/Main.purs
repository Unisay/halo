module Main where

import Preamble

import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, withGraphics)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum, upFromIncluding)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (trim)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Repl (Next(..), Result(..))
import Repl as Repl

main ∷ Effect Unit
main = launchAff_ $ Repl.run Repl.defConfig
  { banner = "Halo"
  , parseCommand = parseCommand
  , evalCommand = evalCommand
  , welcomeMessage = Just $ Array.fold
      let
        color fg = withGraphics (bold <> foreground fg)
      in
        [ "Welcome to "
        , color BrightRed "H"
        , color Green "A"
        , color BrightBlue "L"
        , color BrightMagenta "O"
        ]
  }

data Command = Help | Test

derive instance Eq Command
derive instance Ord Command
derive instance Generic Command _

instance Enum Command where
  pred = genericPred
  succ = genericSucc

instance Show Command where
  show = genericShow

instance Bounded Command where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Command where
  toEnum = genericToEnum
  fromEnum = genericFromEnum
  cardinality = genericCardinality

parseCommand ∷ String → Either String (Maybe Command)
parseCommand command =
  case trim command of
    "help" → Right (Just Help)
    "h" → Right (Just Help)
    "?" → Right (Just Help)
    "test" → Right (Just Test)
    _ → Right Nothing

evalCommand ∷ Command → Effect Result
evalCommand command =
  Ok Continue <$ case command of
    Help → commandHelp
    Test → pure unit

commandHelp ∷ Effect Unit
commandHelp = do
  for_ (upFromIncluding bottom ∷ Array Command) \cmd →
    Console.log $ describeCommand cmd

describeCommand ∷ Command → String
describeCommand = case _ of
  Help → withGraphics bold "help" <> " - outputs this help."
  Test → withGraphics bold "test" <> " - outputs 256 terminal colors."
