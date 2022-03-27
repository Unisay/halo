module Preamble
  ( module Prelude
  , module Data.Maybe
  , module Data.Either
  , pass
  , unwords
  , unlines
  ) where

import Prelude

import Data.Either (Either(..), either, hush, note)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String

pass ∷ ∀ f. Applicative f ⇒ f Unit
pass = pure unit

unwords ∷ Array String → String
unwords = String.joinWith " "

unlines ∷ Array String → String
unlines = String.joinWith "\n"
