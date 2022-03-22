module Repl
  ( runRepl
  , Result(..)
  , Next(..)
  , ReplConfig
  , replConfig
  ) where

import Prelude

import Control.Monad.Rec.Class (untilJust)
import Control.Monad.State.Trans (StateT, evalStateT, gets, modify_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Terminal.Kit as T
import Terminal.Kit.Color (Color8(..), Ground(..), Tone(..))
import Terminal.Kit.Color as C

type ReplM ∷ ∀ k. k → Type → Type
type ReplM c = StateT ReplState Aff

data Result = Error Next | Ok Next

data Next = Continue | Abort

shouldAbort ∷ Result → Boolean
shouldAbort r =
  abortNext case r of
    Error n → n
    Ok n → n
  where
  abortNext = case _ of
    Continue → false
    Abort → true

exitCode ∷ Result → Int
exitCode = case _ of
  Error _ → 1
  Ok _ → 0

data Input a = Skip | Exit | Unknown | Cmd a

type ReplState =
  { history ∷ Array String
  , result ∷ Maybe Result
  }

type ReplConfig c =
  { parseCommand ∷ String → Either String (Maybe c)
  , evalCommand ∷ c → Aff Result
  , banner ∷ String
  , pointer ∷ String
  , unknownCommandMessage ∷ String
  , byeMessage ∷ Maybe String
  }

replConfig ∷ ∀ c. ReplConfig c
replConfig =
  { evalCommand: \_command → pure (Ok Continue)
  , parseCommand: \_command → Right Nothing
  , banner: ""
  , pointer: " ➤ "
  , unknownCommandMessage: "Unknown command."
  , byeMessage: Just "Bye!"
  }

runRepl ∷ ∀ c. ReplConfig c → Aff Unit
runRepl config = evalStateT repl initialState
  where
  initialState ∷ ReplState
  initialState =
    { history: []
    , result: Nothing
    }

  repl ∷ ReplM c Unit
  repl = do
    T.grabInput T.grabInputOptions
    T.onKey \name _match _datum → do
      case name of
        "CTRL_D" → T.releaseInput *> T.print "exit\n"
        "CTRL_C" → T.releaseInput *> T.print "Interrupted" *> T.processExit 0
        _ → pure unit
    loop >>= T.processExit

  loop ∷ ReplM c Int
  loop = untilJust do
    inviteCommand
    result ← readCommand >>= case _ of
      Skip →
        pure $ Ok Continue
      Exit → do
        for_ config.byeMessage T.print
        pure $ Ok Abort
      Unknown → do
        T.printLn config.unknownCommandMessage
        let res = Error Continue
        modify_ _ { result = Just res }
        pure res
      Cmd cmd → do
        res ← liftAff $ config.evalCommand cmd
        modify_ _ { result = Just res }
        pure res
    pure case shouldAbort result of
      true → Just (exitCode result)
      false → Nothing

  readCommand ∷ ReplM c (Input c)
  readCommand = do
    history ← gets _.history
    command ← T.inputField T.inputFieldOptions
      { cancelable = true
      , history = history
      }
    T.nextLine 1
    pure case trim <$> command of
      Nothing → Skip
      Just "exit" → Exit
      Just "quit" → Exit
      Just "" → Skip
      Just line →
        case config.parseCommand line of
          Left _err → Unknown
          Right Nothing → Unknown
          Right (Just c) → Cmd c

  inviteCommand ∷ ReplM c Unit
  inviteCommand = do
    T.print config.banner
    lastCommandResult ← gets $ _.result >>> fromMaybe (Ok Continue)
    C.withColor Fore Dark (statusColor lastCommandResult) \_ →
      T.print config.pointer
    where
    statusColor ∷ Result → Color8
    statusColor = case _ of
      Error _ → Red
      Ok _ → Green
