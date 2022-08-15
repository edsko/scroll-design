{-# LANGUAGE OverloadedStrings #-}

module ScrollDesign.Render.HTML (renderHtml) where

import Prelude hiding (head, div)

import Data.String
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map

import Text.Blaze.Html.Renderer.Pretty qualified as Blaze
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes qualified as A

import ScrollDesign.AST

renderHtml :: Scroll -> String
renderHtml scroll = Blaze.renderHtml $ do
    docTypeHtml $ do
      head $ do
        link ! A.rel  "stylesheet"
             ! A.href "scroll.css"
        title "scroll-design"
      body $
        div ! A.class_ "container" $
          renderScroll initRenderCtxt scroll

{-------------------------------------------------------------------------------
  Rendering context
-------------------------------------------------------------------------------}

data RenderCtxt = RenderCtxt {
      renderCtxtPos   :: Pos
    , renderCtxtSize  :: Int
    , renderCtxtAttrs :: Map AttrName AttrValue
    , renderCtxtLimit :: [Int]
    }

initRenderCtxt :: RenderCtxt
initRenderCtxt = RenderCtxt {
      renderCtxtPos   = Pos { posX = 0, posY = 0 }
    , renderCtxtSize  = 20
    , renderCtxtAttrs = Map.empty
    , renderCtxtLimit = []
    }

{-------------------------------------------------------------------------------
  Renderer proper
-------------------------------------------------------------------------------}

renderScroll :: RenderCtxt -> Scroll -> Html
renderScroll ctxt = \case
    Text t ->
      renderText ctxt t
    Elements ks ->
      mapM_ (renderScroll ctxt) ks
    SetPos pos k ->
      renderScroll ctxt{renderCtxtPos = pos} k
    SetSize size k ->
      renderScroll ctxt{renderCtxtSize = size} k
    SetAttr name value k -> do
      let attrs' = Map.insert name value (renderCtxtAttrs ctxt)
      renderScroll ctxt{renderCtxtAttrs = attrs'} k
    SetLimit limit k ->
      renderScroll ctxt{renderCtxtLimit = limit} k

renderText :: RenderCtxt -> String -> Html
renderText RenderCtxt{renderCtxtSize = size, ..} =
    go renderCtxtLimit renderCtxtPos
  where
    go :: [Int] -> Pos -> String -> Html
    go _     _   []     = return ()
    go limit pos (c:cs) = do
        div ! A.class_ "scrollElement"
            ! A.style (renderAttrs $ attrs pos) $
          fromString [c]
        uncurry go (updatePos limit pos) cs

    updatePos :: [Int] -> Pos -> ([Int], Pos)
    updatePos []     (Pos x y) = ([]       , Pos x (y + size))
    updatePos (1:ls) (Pos x _) = (ls       , Pos (x - size) (posY renderCtxtPos))
    updatePos (l:ls) (Pos x y) = ((l-1):ls , Pos x (y + size))

    attrs :: Pos -> [(AttrName, AttrValue)]
    attrs (Pos x y) = concat [
          Map.toList renderCtxtAttrs
        , [ ("left"   , show x    `WithUnit` "px")
          , ("top"    , show y    `WithUnit` "px")
          , ("width"  , show size `WithUnit` "px")
          , ("height" , show size `WithUnit` "px")
          ]
        ]

renderAttrs :: [(AttrName, AttrValue)] -> AttributeValue
renderAttrs = fromString . intercalate " " . map (uncurry aux)
  where
    aux :: AttrName -> AttrValue -> String
    aux prop (Unitless value)        = prop ++ ": " ++ value ++ ";"
    aux prop (value `WithUnit` unit) = prop ++ ": " ++ value ++ unit ++ ";"
