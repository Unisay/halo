module Readline where

import Prelude

import Data.Either (Either(..))
import Data.Nullable (Nullable, notNull, null)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (makeAff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3)
import Node.Process (stdin, stdout)
import Node.Stream (Readable, Writable)

data Interface

data Completer

foreign import createCompleter
  ∷ (String → { entries ∷ Array String, substring ∷ String }) → Completer

noCompleter ∷ Completer
noCompleter = createCompleter \substring → { entries: [], substring }

type InterfaceOptions =
  { input ∷ Readable ()
  -- ^ The Readable stream to listen to.

  , output ∷ Writable ()
  -- ^ The Writable stream to write readline data to.

  , completer ∷ Completer
  -- ^ An optional function used for Tab autocompletion.

  , terminal ∷ Nullable Boolean
  -- ^ true if the input and output streams should be treated like a TTY,
  -- and have ANSI/VT100 escape codes written to it.
  -- Default: checking isTTY on the output stream upon instantiation.

  , history ∷ Array String
  -- ^ Initial list of history lines. This option makes sense only if terminal
  -- is set to true by the user or by an internal output check,
  -- otherwise the history caching mechanism is not initialized at all.

  , historySize ∷ Int
  -- ^ Maximum number of history lines retained. To disable the history set
  -- this value to 0. This option makes sense only if terminal is set to true by
  -- the user or by an internal output check, otherwise the history caching
  -- mechanism is not initialized at all. 

  , removeHistoryDuplicates ∷ Boolean
  -- ^ If true, when a new input line added to the history list duplicates
  -- an older one, this removes the older line from the list. 

  , prompt ∷ String
  -- ^ The prompt string to use. 

  , crlfDelay ∷ Milliseconds
  -- ^ If the delay between \r and \n exceeds crlfDelay milliseconds,
  -- both \r and \n will be treated as separate end-of-line input.
  -- crlfDelay will be coerced to a number no less than 100.
  -- It can be set to Infinity, in which case \r followed by \n will always
  -- be considered a single newline (which may be reasonable for reading files
  -- with \r\n line delimiter).

  , escapeCodeTimeout ∷ Milliseconds
  -- ^ The duration readline will wait for a character (when reading an
  -- ambiguous key sequence in milliseconds one that can both form a complete
  -- key sequence using the input read so far and can take additional input to
  -- complete a longer key sequence).

  , tabSize ∷ Int
  -- ^ The number of spaces a tab is equal to (minimum 1).
  }

interfaceOptions ∷ Readable () → Writable () → InterfaceOptions
interfaceOptions input output =
  { input
  , output
  , completer: noCompleter
  , terminal: null
  , history: []
  , historySize: 30
  , removeHistoryDuplicates: false
  , prompt: "> "
  , crlfDelay: Milliseconds 100.0
  , escapeCodeTimeout: Milliseconds 500.0
  , tabSize: 8
  }

stdIOInterface ∷ InterfaceOptions
stdIOInterface = interfaceOptions stdin stdout

foreign import _createInterface ∷ EffectFn1 InterfaceOptions Interface

createInterface ∷ ∀ m. MonadEffect m ⇒ InterfaceOptions → m Interface
createInterface = liftEffect <<< runEffectFn1 _createInterface

foreign import _close ∷ EffectFn1 Interface Unit

close ∷ ∀ m. MonadEffect m ⇒ Interface → m Unit
close = liftEffect <<< runEffectFn1 _close

type PreserveCursor = Boolean

foreign import _prompt ∷ EffectFn2 Interface PreserveCursor Unit

prompt ∷ ∀ m. MonadEffect m ⇒ Interface → PreserveCursor → m Unit
prompt iface = liftEffect <<< runEffectFn2 _prompt iface

foreign import _setPrompt ∷ EffectFn2 Interface String Unit

setPrompt ∷ ∀ m. MonadEffect m ⇒ Interface → String → m Unit
setPrompt iface = liftEffect <<< runEffectFn2 _setPrompt iface

foreign import _getPrompt ∷ EffectFn1 Interface String

getPrompt ∷ ∀ m. MonadEffect m ⇒ Interface → m String
getPrompt = liftEffect <<< runEffectFn1 _getPrompt

foreign import _question
  ∷ EffectFn3 Interface String (EffectFn1 String Unit) (Effect Unit)

question ∷ ∀ m. MonadAff m ⇒ Interface → String → m String
question iface text = liftAff do
  makeAff \next → do
    cancel ← runEffectFn3 _question iface text $ mkEffectFn1 $ next <<< Right
    pure $ Aff.effectCanceler cancel

foreign import _onLine ∷ EffectFn2 Interface (EffectFn1 String Unit) Unit

onLine ∷ ∀ m. MonadEffect m ⇒ Interface → (String → Effect Unit) → m Unit
onLine iface = liftEffect <<< runEffectFn2 _onLine iface <<< mkEffectFn1

readLine ∷ ∀ m. MonadAff m ⇒ Interface → m String
readLine iface = liftAff do
  makeAff \next → do
    onLine iface $ next <<< Right
    pure $ Aff.nonCanceler

foreign import _onHistory ∷ EffectFn2 Interface (EffectFn1 String Unit) Unit

onHistory ∷ ∀ m. MonadEffect m ⇒ Interface → (String → Effect Unit) → m Unit
onHistory iface = liftEffect <<< runEffectFn2 _onHistory iface <<< mkEffectFn1

type Key =
  { ctrl ∷ Boolean
  , meta ∷ Boolean
  , shift ∷ Boolean
  , name ∷ String
  }

type KeyEvent =
  { ctrl ∷ Boolean
  , meta ∷ Boolean
  , shift ∷ Boolean
  , name ∷ String
  , sequence ∷ String
  }

foreign import _write
  ∷ EffectFn3 (Writable ()) (Nullable String) (Nullable Key) Unit

write ∷ ∀ m. MonadEffect m ⇒ Writable () → String → m Unit
write stream s = liftEffect $ runEffectFn3 _write stream (notNull s) null

writeKey ∷ ∀ m. MonadEffect m ⇒ Writable () → Key → m Unit
writeKey stream = liftEffect <<< runEffectFn3 _write stream null <<< notNull

ctrl ∷ String → Key
ctrl name = { ctrl: true, meta: false, shift: false, name }

meta ∷ String → Key
meta name = { ctrl: false, meta: true, shift: false, name }

shift ∷ String → Key
shift name = { ctrl: false, meta: false, shift: true, name }

foreign import _emitKeypressEvents ∷ EffectFn1 (Readable ()) Unit

foreign import _onKeypress
  ∷ EffectFn2 (Readable ()) (EffectFn2 String KeyEvent Unit) Unit

emitKeypressEvents ∷ ∀ m. MonadEffect m ⇒ Readable () → m Unit
emitKeypressEvents = liftEffect <<< runEffectFn1 _emitKeypressEvents

onKeyPress
  ∷ ∀ m
  . MonadEffect m
  ⇒ Readable ()
  → (String → KeyEvent → Effect Unit)
  → m Unit
onKeyPress stream = liftEffect
  <<< runEffectFn2 _onKeypress stream
  <<< mkEffectFn2

onKeyPressEvent
  ∷ ∀ m
  . MonadEffect m
  ⇒ Readable ()
  → (String → KeyEvent → Effect Unit)
  → m Unit
onKeyPressEvent stream callback = do
  emitKeypressEvents stream
  onKeyPress stream callback

foreign import _dir ∷ ∀ a. EffectFn1 a Unit

dir ∷ ∀ a. a → Effect Unit
dir = runEffectFn1 _dir
