module Repl
  ( run
  , Result(..)
  , Next(..)
  , ReplConfig
  , defConfig
  ) where

import Preamble

import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, withGraphics)
import Control.Monad.Rec.Class (class MonadRec, untilJust)
import Control.Monad.State.Class (get)
import Control.Monad.State.Trans (class MonadState, StateT, evalStateT, gets, modify_)
import Data.Newtype (class Newtype, unwrap)
import Data.String (trim)
import Data.String as String
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Process as Process
import Readline as Readline

newtype ReplT c m a = ReplT (StateT (ReplState c) m a)

derive instance Newtype (ReplT c m a) _
derive newtype instance Functor m ⇒ Functor (ReplT c m)
derive newtype instance Monad m ⇒ Apply (ReplT c m)
derive newtype instance Monad m ⇒ Applicative (ReplT c m)
derive newtype instance Monad m ⇒ Bind (ReplT c m)
instance Monad m ⇒ Monad (ReplT c m)
derive newtype instance MonadRec m ⇒ MonadRec (ReplT c m)
derive newtype instance Monad m ⇒ MonadState (ReplState c) (ReplT c m)
derive newtype instance MonadEffect m ⇒ MonadEffect (ReplT c m)
derive newtype instance MonadAff m ⇒ MonadAff (ReplT c m)

type Repl c = ReplT c Aff

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

type ReplState c =
  { config ∷ ReplConfig c
  , history ∷ Array String
  , result ∷ Maybe Result
  , interface ∷ Readline.Interface
  }

type ReplConfig c =
  { parseCommand ∷ String → Either String (Maybe c)
  , evalCommand ∷ c → Effect Result
  , banner ∷ String
  , pointer ∷ String
  , unknownCommandMessage ∷ String
  , welcomeMessage ∷ Maybe String
  , byeMessage ∷ Maybe String
  }

defConfig ∷ ∀ c. ReplConfig c
defConfig =
  { evalCommand: \_command → pure (Ok Continue)
  , parseCommand: \_command → Right Nothing
  , banner: ""
  , pointer: " ➤ " -- ➜❯
  , unknownCommandMessage: "Unknown command"
  , welcomeMessage: Nothing
  , byeMessage: Just "Bye!"
  }

run ∷ ∀ c. ReplConfig c → Aff Unit
run config = liftAff $ bracket initIface disposeIface \interface → do
  evalStateT (unwrap repl) { config, history: [], result: Nothing, interface }

  where
  initIface ∷ Aff Readline.Interface
  initIface = Readline.createInterface Readline.stdIOInterface
    { prompt = config.banner <> config.pointer
    , completer = Readline.createCompleter \s →
        case String.trim s of
          "q" → { entries: [ "quit" ], substring: "q" }
          substring → { entries: [], substring }
    }

  disposeIface ∷ Readline.Interface → Aff Unit
  disposeIface = Readline.close

repl ∷ ∀ c. Repl c Unit
repl = do
  gets _.config.welcomeMessage >>= maybe pass printLn
  code ← loop
  liftEffect $ Process.exit code

loop ∷ ∀ c. Repl c Int
loop = untilJust do
  { config, interface } ← get
  lastCommandResult ← gets $ _.result >>> fromMaybe (Ok Continue)
  modify_ _ { result = Just (Ok Continue) }
  let
    fg = case lastCommandResult of
      Ok _ → BrightGreen
      Error _ → BrightRed
  Readline.setPrompt interface $ withGraphics bold config.banner
    <> withGraphics (bold <> foreground fg) config.pointer
  Readline.prompt interface true
  reply ← Readline.readLine interface
  result ← handleReply reply >>= case _ of
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
      res ← liftEffect $ config.evalCommand cmd
      modify_ _ { result = Just res }
      pure res
  pure case shouldAbort result of
    true → Just (exitCode result)
    false → Nothing

handleReply ∷ ∀ c. String → Repl c (Input c)
handleReply reply = do
  { config } ← get
  pure case trim reply of
    "exit" → Exit
    "quit" → Exit
    "" → Skip
    line → case config.parseCommand line of
      Left _err → Unknown
      Right Nothing → Unknown
      Right (Just c) → Cmd c

print ∷ ∀ m. MonadEffect m ⇒ String → m Unit
print = Readline.write Process.stdout

printLn ∷ ∀ m. MonadEffect m ⇒ String → m Unit
printLn = print <<< flip append "\n"
