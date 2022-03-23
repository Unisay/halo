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
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

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
  repl = loop >>= liftEffect <<< Process.exit

  loop ∷ ReplM c Int
  loop = untilJust do
    inviteCommand
    result ← readCommand >>= case _ of
      Skip →
        pure $ Ok Continue
      Exit → do
        for_ config.byeMessage Console.log
        pure $ Ok Abort
      Unknown → do
        printLn config.unknownCommandMessage
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
    -- command ← T.inputField T.inputFieldOptions
    --   { cancelable = true
    --   , history = history
    --   }
    let command = unsafeCoerce unit
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
    Console.log config.banner
    lastCommandResult ← gets $ _.result >>> fromMaybe (Ok Continue)
    Console.log config.pointer

printLn ∷ ∀ m. MonadEffect m ⇒ String → m Unit
printLn s = void $ liftEffect $ Stream.writeString Process.stdout UTF8 s
  (pure unit)
