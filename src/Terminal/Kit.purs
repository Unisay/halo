-- | https://github.com/cronvel/terminal-kit/blob/HEAD/doc/documentation.md#ref.TOC
module Terminal.Kit
  ( fullscreen'
  , fullscreen
  , getCursorLocation'
  , getCursorLocation
  , processExit'
  , processExit
  , grabInputOptions
  , grabInput'
  , grabInput
  , releaseInput'
  , releaseInput
  , onKey
  , print'
  , print
  , printLn
  , printShow
  , reset'
  , reset
  , width'
  , width
  , height'
  , height
  , InputFieldOptions
  , inputField
  , inputFieldOptions
  , nextLine'
  , nextLine
  , previousLine'
  , previousLine
  ) where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn3, runEffectFn1, runEffectFn2)
import Terminal.Kit.Terminal (Terminal, defaultTerminal)

foreign import _fullscreen ∷ EffectFn2 Terminal Boolean Unit

fullscreen' ∷ ∀ m. MonadEffect m ⇒ Terminal → Boolean → m Unit
fullscreen' term = liftEffect <<< runEffectFn2 _fullscreen term

fullscreen ∷ ∀ m. MonadEffect m ⇒ Boolean → m Unit
fullscreen = fullscreen' defaultTerminal

type Location = { x ∷ Int, y ∷ Int }

foreign import _getCursorLocation ∷ EffectFn1 Terminal (Promise Location)

getCursorLocation' ∷ ∀ m. MonadAff m ⇒ Terminal → m Location
getCursorLocation' term = liftAff do
  liftEffect (runEffectFn1 _getCursorLocation term) >>= toAff

getCursorLocation ∷ ∀ m. MonadAff m ⇒ m Location
getCursorLocation = getCursorLocation' defaultTerminal

foreign import _processExit ∷ ∀ a. EffectFn2 Terminal Int a

processExit' ∷ ∀ a m. MonadEffect m ⇒ Terminal → Int → m a
processExit' term code = liftEffect $ runEffectFn2 _processExit term code

processExit ∷ ∀ a m. MonadEffect m ⇒ Int → m a
processExit = processExit' defaultTerminal

type GrabInputOptions = { mouse ∷ Nullable String, focus ∷ Nullable Boolean }

grabInputOptions ∷ GrabInputOptions
grabInputOptions = { mouse: Nullable.null, focus: Nullable.null }

foreign import _grabInput ∷ EffectFn2 Terminal GrabInputOptions Unit

grabInput' ∷ ∀ m. MonadEffect m ⇒ Terminal → GrabInputOptions → m Unit
grabInput' term opts = liftEffect $ runEffectFn2 _grabInput term opts

grabInput ∷ ∀ m. MonadEffect m ⇒ GrabInputOptions → m Unit
grabInput = grabInput' defaultTerminal

foreign import _releaseInput ∷ EffectFn1 Terminal Unit

releaseInput' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
releaseInput' = liftEffect <<< runEffectFn1 _releaseInput

releaseInput ∷ ∀ m. MonadEffect m ⇒ m Unit
releaseInput = releaseInput' defaultTerminal

type KeyEvent = { isCharacter ∷ Boolean, codepoint ∷ Nullable Int }

foreign import _onKey
  ∷ EffectFn2 Terminal (EffectFn3 String (Array String) KeyEvent Unit) Unit

onKey
  ∷ ∀ m
  . MonadEffect m
  ⇒ (String → Array String → KeyEvent → Effect Unit)
  → m Unit
onKey callback = liftEffect
  $ runEffectFn2 _onKey defaultTerminal
  $ mkEffectFn3 callback

foreign import _print ∷ EffectFn2 Terminal String Unit

print' ∷ ∀ m. MonadEffect m ⇒ Terminal → String → m Unit
print' term s = liftEffect $ runEffectFn2 _print term s

print ∷ ∀ m. MonadEffect m ⇒ String → m Unit
print = print' defaultTerminal

printShow ∷ ∀ m s. MonadEffect m ⇒ Show s ⇒ s → m Unit
printShow = print <<< show

printLn' ∷ ∀ m. MonadEffect m ⇒ Terminal → String → m Unit
printLn' term str = print' term str *> nextLine' term 1

printLn ∷ ∀ m. MonadEffect m ⇒ String → m Unit
printLn = printLn' defaultTerminal

foreign import _nextLine ∷ EffectFn2 Terminal Int Unit

nextLine' ∷ ∀ m. MonadEffect m ⇒ Terminal → Int → m Unit
nextLine' term n = liftEffect $ runEffectFn2 _nextLine term n

nextLine ∷ ∀ m. MonadEffect m ⇒ Int → m Unit
nextLine = nextLine' defaultTerminal

foreign import _previousLine ∷ EffectFn2 Terminal Int Unit

previousLine' ∷ ∀ m. MonadEffect m ⇒ Terminal → Int → m Unit
previousLine' term n = liftEffect $ runEffectFn2 _previousLine term n

previousLine ∷ ∀ m. MonadEffect m ⇒ Int → m Unit
previousLine = previousLine' defaultTerminal

foreign import _reset ∷ EffectFn1 Terminal Unit

reset' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
reset' = liftEffect <<< runEffectFn1 _reset

reset ∷ ∀ m. MonadEffect m ⇒ m Unit
reset = reset' defaultTerminal

foreign import _width ∷ EffectFn1 Terminal Int

width' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Int
width' = liftEffect <<< runEffectFn1 _width

width ∷ ∀ m. MonadEffect m ⇒ m Int
width = width' defaultTerminal

foreign import _height ∷ EffectFn1 Terminal Int

height' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Int
height' = liftEffect <<< runEffectFn1 _height

height ∷ ∀ m. MonadEffect m ⇒ m Int
height = height' defaultTerminal

type InputFieldOptions =
  { x ∷ Nullable Int
  , y ∷ Nullable Int
  , echoChar ∷ Nullable Boolean
  , echo ∷ Boolean
  , default ∷ String
  , cursorPosition ∷ Int
  , cancelable ∷ Boolean
  , maxLength ∷ Nullable Int
  , minLength ∷ Nullable Int
  , history ∷ Array String
  }

foreign import inputFieldOptions ∷ InputFieldOptions

foreign import _inputField
  ∷ EffectFn2 Terminal InputFieldOptions (Promise (Nullable String))

inputField'
  ∷ ∀ m
  . MonadAff m
  ⇒ Terminal
  → InputFieldOptions
  → m (Maybe String)
inputField' term opts = liftAff do
  Nullable.toMaybe <$> do
    liftEffect (runEffectFn2 _inputField term opts) >>= toAff

inputField ∷ ∀ m. MonadAff m ⇒ InputFieldOptions → m (Maybe String)
inputField = inputField' defaultTerminal
