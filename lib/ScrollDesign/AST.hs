module ScrollDesign.AST (
    Scroll(..)
  , Pos(..)
  , Delta(..)
  , AttrName
  , AttrValue(..)
  ) where

data Pos = Pos { posX :: Int, posY :: Int }

data Delta = Delta { deltaX :: Int, deltaY :: Int }

type AttrName = String

data AttrValue = Unitless String | WithUnit String String

data Scroll =
    Text String
  | Elements [Scroll]
  | SetPos Pos Scroll
  | SetSize Int Scroll
  | SetAttr AttrName AttrValue Scroll
  | SetLimit [Int] Scroll
