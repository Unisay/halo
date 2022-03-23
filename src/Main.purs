module Main where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, upFromIncluding)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (trim)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Node.Process as Process
import Readline (interfaceOptions)
import Readline as Readline
import Repl (Next(..), Result(..))

main ∷ Effect Unit
main = launchAff_ do
  iface ← Readline.createInterface
    (interfaceOptions Process.stdin Process.stdout)
      { prompt = ">>> "
      }
  Readline.onLine iface \line → do
    Console.log line
    case line of
      "quit" → Readline.close iface
      _ → pure unit

-- runRepl replConfig
--   { banner = "Halo"
--   , parseCommand = parseCommand
--   , evalCommand = evalCommand
--   }

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

evalCommand ∷ Command → Aff Result
evalCommand command =
  Ok Continue <$ case command of
    Help → commandHelp
    Test → pure unit

commandHelp ∷ Aff Unit
commandHelp = do
  for_ (upFromIncluding bottom ∷ Array Command) \cmd →
    Console.log $ describeCommand cmd

describeCommand ∷ Command → String
describeCommand = case _ of
  Help → "^bhelp^ - outputs this help."
  Test → "^btest^ - outputs 256 terminal colors."
