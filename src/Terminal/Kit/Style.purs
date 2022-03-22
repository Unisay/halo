module Terminal.Kit.Style
  ( styleReset'
  , styleReset
  , bold'
  , bold
  , dim'
  , dim
  , italic'
  , italic
  , underline'
  , underline
  , blink'
  , blink
  , inverse'
  , inverse
  , hidden'
  , hidden
  , strike'
  , strike
  ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Terminal.Kit.Terminal (Terminal, defaultTerminal)

foreign import _styleReset ∷ EffectFn1 Terminal Unit

styleReset' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
styleReset' = liftEffect <<< runEffectFn1 _styleReset

styleReset ∷ ∀ m. MonadEffect m ⇒ m Unit
styleReset = styleReset' defaultTerminal

foreign import _bold ∷ EffectFn1 Terminal Unit

bold' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
bold' = liftEffect <<< runEffectFn1 _bold

bold ∷ ∀ m. MonadEffect m ⇒ m Unit
bold = bold' defaultTerminal

foreign import _dim ∷ EffectFn1 Terminal Unit

dim' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
dim' = liftEffect <<< runEffectFn1 _dim

dim ∷ ∀ m. MonadEffect m ⇒ m Unit
dim = dim' defaultTerminal

foreign import _italic ∷ EffectFn1 Terminal Unit

italic' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
italic' = liftEffect <<< runEffectFn1 _italic

italic ∷ ∀ m. MonadEffect m ⇒ m Unit
italic = italic' defaultTerminal

foreign import _underline ∷ EffectFn1 Terminal Unit

underline' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
underline' = liftEffect <<< runEffectFn1 _underline

underline ∷ ∀ m. MonadEffect m ⇒ m Unit
underline = underline' defaultTerminal

foreign import _blink ∷ EffectFn1 Terminal Unit

blink' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
blink' = liftEffect <<< runEffectFn1 _blink

blink ∷ ∀ m. MonadEffect m ⇒ m Unit
blink = blink' defaultTerminal

foreign import _inverse ∷ EffectFn1 Terminal Unit

inverse' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
inverse' = liftEffect <<< runEffectFn1 _inverse

inverse ∷ ∀ m. MonadEffect m ⇒ m Unit
inverse = inverse' defaultTerminal

foreign import _hidden ∷ EffectFn1 Terminal Unit

hidden' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
hidden' = liftEffect <<< runEffectFn1 _hidden

hidden ∷ ∀ m. MonadEffect m ⇒ m Unit
hidden = hidden' defaultTerminal

foreign import _strike ∷ EffectFn1 Terminal Unit

strike' ∷ ∀ m. MonadEffect m ⇒ Terminal → m Unit
strike' = liftEffect <<< runEffectFn1 _strike

strike ∷ ∀ m. MonadEffect m ⇒ m Unit
strike = strike' defaultTerminal
