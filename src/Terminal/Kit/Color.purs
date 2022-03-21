module Terminal.Kit.Color
  ( Tone(..)
  , Ground(..)
  , Color8(..)
  , color
  , color16'
  , color16
  , color256'
  , color256
  , colorRgb'
  , colorRgb
  , defaultColor'
  , defaultColor
  , withColor
  ) where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import Terminal.Kit.Terminal (Terminal, defaultTerminal)

data Tone = Dark | Bright

derive instance Generic Tone _
derive instance Eq Tone
derive instance Ord Tone
instance Enum Tone where
  pred = genericPred
  succ = genericSucc

instance Bounded Tone where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Tone where
  toEnum = genericToEnum
  fromEnum = genericFromEnum
  cardinality = genericCardinality

instance Show Tone where
  show = genericShow

data Color8
  = Black
  | Blue
  | Green
  | Cyan
  | Red
  | Magenta
  | Yellow
  | White

derive instance Generic Color8 _
derive instance Eq Color8
derive instance Ord Color8
instance Enum Color8 where
  pred = genericPred
  succ = genericSucc

instance Bounded Color8 where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Color8 where
  toEnum = genericToEnum
  fromEnum = genericFromEnum
  cardinality = genericCardinality

instance Show Color8 where
  show = genericShow

foreign import _fgColor16 ∷ Terminal → Int → Effect Unit
foreign import _fgColor256 ∷ Terminal → Int → Effect Unit
foreign import _bgColor16 ∷ Terminal → Int → Effect Unit
foreign import _bgColor256 ∷ Terminal → Int → Effect Unit
foreign import _fgDefaultColor ∷ Terminal → Effect Unit
foreign import _bgDefaultColor ∷ Terminal → Effect Unit
foreign import _fgColorRgb ∷ EffectFn4 Terminal Number Number Number Unit
foreign import _bgColorRgb ∷ EffectFn4 Terminal Number Number Number Unit

color8int ∷ Tone → Color8 → Int
color8int tone col =
  toneInt tone + case col of
    Black → 0
    Red → 1
    Yellow → 3
    Blue → 4
    Green → 2
    Magenta → 5
    Cyan → 6
    White → 7
  where
  toneInt = case _ of
    Dark → 0
    Bright → 8

color' ∷ ∀ m. MonadEffect m ⇒ Terminal → Ground → Tone → Color8 → m Unit
color' term ground tone c = liftEffect $ color16' term ground $ color8int tone c

color ∷ ∀ m. MonadEffect m ⇒ Ground → Tone → Color8 → m Unit
color = color' defaultTerminal

withColor
  ∷ ∀ m
  . MonadEffect m
  ⇒ Ground
  → Tone
  → Color8
  → (Unit → m Unit)
  → m Unit
withColor g t c a = do
  color g t c
  a unit
  defaultColor g

color16' ∷ ∀ m. MonadEffect m ⇒ Terminal → Ground → Int → m Unit
color16' term ground c = liftEffect $ _color16 term c
  where
  _color16 = case ground of
    Back → _bgColor16
    Fore → _fgColor16

color16 ∷ ∀ m. MonadEffect m ⇒ Ground → Int → m Unit
color16 = color16' defaultTerminal

color256' ∷ ∀ m. MonadEffect m ⇒ Terminal → Ground → Int → m Unit
color256' term ground c = liftEffect $ _color256 term c
  where
  _color256 = case ground of
    Back → _bgColor256
    Fore → _fgColor256

color256 ∷ ∀ m. MonadEffect m ⇒ Ground → Int → m Unit
color256 = color256' defaultTerminal

colorRgb'
  ∷ ∀ m
  . MonadEffect m
  ⇒ Terminal
  → Ground
  → Number
  → Number
  → Number
  → m Unit
colorRgb' term ground r g b = liftEffect $ runEffectFn4 _colorRgb term r g b
  where
  _colorRgb = case ground of
    Back → _bgColorRgb
    Fore → _fgColorRgb

colorRgb
  ∷ ∀ m
  . MonadEffect m
  ⇒ Ground
  → Number
  → Number
  → Number
  → m Unit
colorRgb = colorRgb' defaultTerminal

defaultColor' ∷ ∀ m. MonadEffect m ⇒ Terminal → Ground → m Unit
defaultColor' term ground =
  liftEffect case ground of
    Back → _bgDefaultColor term
    Fore → _fgDefaultColor term

defaultColor ∷ ∀ m. MonadEffect m ⇒ Ground → m Unit
defaultColor = defaultColor' defaultTerminal

data Ground = Back | Fore

derive instance Generic Ground _
derive instance Eq Ground
derive instance Ord Ground
instance Enum Ground where
  pred = genericPred
  succ = genericSucc

instance Bounded Ground where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Ground where
  toEnum = genericToEnum
  fromEnum = genericFromEnum
  cardinality = genericCardinality

instance Show Ground where
  show = genericShow
