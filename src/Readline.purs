module Readline where

import Prelude

import Data.Nullable (Nullable, null)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried
  ( EffectFn1
  , EffectFn2
  , mkEffectFn1
  , runEffectFn1
  , runEffectFn2
  )
import Node.Stream (Readable, Writable)

data Interface

data Completer

foreign import _createCompleter
  ∷ (String → { entries ∷ Array String, substring ∷ String }) → Completer

noCompleter ∷ Completer
noCompleter = _createCompleter \substring → { entries: [], substring }

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

foreign import _createInterface ∷ EffectFn1 InterfaceOptions Interface

createInterface ∷ ∀ m. MonadEffect m ⇒ InterfaceOptions → m Interface
createInterface = liftEffect <<< runEffectFn1 _createInterface

foreign import _close ∷ EffectFn1 Interface Unit

close ∷ ∀ m. MonadEffect m ⇒ Interface → m Unit
close = liftEffect <<< runEffectFn1 _close

foreign import _onLine ∷ EffectFn2 Interface (EffectFn1 String Unit) Unit

onLine ∷ ∀ m. MonadEffect m ⇒ Interface → (String → Effect Unit) → m Unit
onLine iface = liftEffect <<< runEffectFn2 _onLine iface <<< mkEffectFn1

