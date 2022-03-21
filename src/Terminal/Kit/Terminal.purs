module Terminal.Kit.Terminal (Terminal, defaultTerminal) where

data Terminal

foreign import defaultTerminal ∷ Terminal
