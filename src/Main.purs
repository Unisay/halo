module Main where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum, enumFromTo, upFromIncluding)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Data.String (trim)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Terminal.Kit as T
import Terminal.Kit.Color (Color8(..), Ground(..), Tone(..))
import Terminal.Kit.Color as C

main ∷ Effect Unit
main = launchAff_ (loop Ok)
  where
  loop r = do
    T.print "Halo"
    C.withColor Fore Dark (statusColor r) \_ → T.print " ➤ "
    readCommand >>= case _ of
      Help → commandHelp *> loop Ok
      Test → commandTest *> loop Ok
      Skip → loop Ok
      Exit → T.processExit 0
      Unknown → T.printLn "Unknown command" *> loop Error

statusColor ∷ Result → Color8
statusColor = case _ of
  Error → Red
  Ok → Green

data Result = Error | Ok

data Command = Skip | Help | Exit | Test | Unknown

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

readCommand ∷ Aff Command
readCommand = do
  command ← T.inputField T.inputFieldOptions { cancelable = true }
  T.nextLine 1
  pure case maybe "" trim command of
    "help" → Help
    "h" → Help
    "?" → Help
    "exit" → Exit
    "quit" → Exit
    "test" → Test
    "" → Skip
    _ → Unknown

commandHelp ∷ Aff Unit
commandHelp = do
  T.printLn "Available commands:"
  for_ (upFromIncluding bottom ∷ Array Command) \cmd →
    T.printLn $ " - " <> show cmd

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

