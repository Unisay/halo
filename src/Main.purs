module Main where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, enumFromTo, upFromIncluding)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (trim)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Repl (Next(..), Result(..), replConfig, runRepl)
import Terminal.Kit as T
import Terminal.Kit.Color (Color8(..), Ground(..), Tone(..))
import Terminal.Kit.Color as C

main ∷ Effect Unit
main = launchAff_ do
  runRepl replConfig
    { banner = "Halo"
    , parseCommand = parseCommand
    , evalCommand = evalCommand
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

evalCommand ∷ Command → Aff Result
evalCommand command =
  Ok Continue <$ case command of
    Help → commandHelp
    Test → commandTest

commandHelp ∷ Aff Unit
commandHelp = do
  T.printLn "^+Available commands:^-"
  for_ (upFromIncluding bottom ∷ Array Command) \cmd →
    T.printLn $ describeCommand cmd

describeCommand ∷ Command → String
describeCommand = case _ of
  Help → "^bhelp^ - outputs this help."
  Test → "^btest^ - outputs 256 terminal colors."

commandTest ∷ Aff Unit
commandTest = liftEffect do
  T.reset
  C.withColor Back Dark Green \_ → C.withColor Fore Dark Black \_ →
    T.print " Width: "
  T.printShow =<< T.width
  C.withColor Fore Dark Green \_ → T.print "\nHeight: "
  T.printShow =<< T.height
  T.print "\n"
  for_ [ Back, Fore ] \g → do
    for_ [ Dark, Bright ] \t →
      for_ (enumFromTo Black White ∷ Array _) \c → do
        C.withColor g t c \_ → do
          T.printShow t
          T.printShow c
        T.print " "
    for_ (enumFromTo 0 255 ∷ Array _) \c → do
      C.color256 g c
      T.printShow c
      C.defaultColor g
      T.print " "

