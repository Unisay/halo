module Main where

import Prelude

import Dodo (Doc, plainText, print, text, twoSpaces)
import Effect (Effect)
import Effect.Console as Console

main ∷ Effect Unit
main = Console.log $ print plainText twoSpaces hello

hello ∷ ∀ a. Doc a
hello = text "Hello, " <> text "World!"
