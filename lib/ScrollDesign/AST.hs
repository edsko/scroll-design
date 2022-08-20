module ScrollDesign.AST (
    Scroll(..)
  ) where

data Scroll =
    Text String
  | Div String [Scroll]
  | Elements [Scroll]
  | SetSize Int Scroll
  | SetLimit [Int] Scroll
  | SetWiggle Int Scroll
