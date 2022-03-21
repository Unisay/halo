module Repl
  ( runRepl
  , Result(..)
  , ReplConfig
  , replConfig
  ) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Trans (StateT, evalStateT, gets, modify_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Terminal.Kit as T
import Terminal.Kit.Color (Color8(..), Ground(..), Tone(..))
import Terminal.Kit.Color as C

type ReplM ∷ ∀ k. k → Type → Type
type ReplM c = StateT ReplState Aff

data Result = Error | Ok

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
  }

replConfig ∷ ∀ c. ReplConfig c
replConfig =
  { evalCommand: const (pure Ok)
  , parseCommand: const (Right Nothing)
  , banner: ""
  , pointer: " ➤ "
  }

runRepl ∷ ∀ c. ReplConfig c → Aff Unit
runRepl config = void $ evalStateT repl initialState
  where
  initialState ∷ ReplState
  initialState =
    { history: []
    , result: Nothing
    }

  repl ∷ ReplM c Result
  repl = forever do
    inviteCommand
    readCommand >>= case _ of
      Skip → pure Ok
      Exit → T.processExit 0
      Unknown → T.printLn "Unknown command" $> Error
      Cmd cmd → do
        res ← liftAff $ config.evalCommand cmd
        modify_ _ { result = Just res }
        pure res

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
    lastCommandResult ← gets $ _.result >>> fromMaybe Ok
    C.withColor Fore Dark (statusColor lastCommandResult) \_ →
      T.print config.pointer
    where
    statusColor ∷ Result → Color8
    statusColor = case _ of
      Error → Red
      Ok → Green
