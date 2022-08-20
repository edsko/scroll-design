{-# LANGUAGE OverloadedStrings #-}

module ScrollDesign.Render.HTML (renderHtml) where

import Prelude hiding (id, head, div)

import Data.List (intercalate)
import Data.String
import System.Random.Stateful

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
        runStateGen_ (mkStdGen 0) $ \gen ->
          renderScroll gen initRenderCtxt scroll

{-------------------------------------------------------------------------------
  Rendering context
-------------------------------------------------------------------------------}

data RenderCtxt = RenderCtxt {
      renderCtxtSize   :: Int
    , renderCtxtLimit  :: [Int]
    , renderCtxtWiggle :: Int
    }

initRenderCtxt :: RenderCtxt
initRenderCtxt = RenderCtxt {
      renderCtxtSize   = 20
    , renderCtxtLimit  = []
    , renderCtxtWiggle = 0
    }

{-------------------------------------------------------------------------------
  Renderer proper
-------------------------------------------------------------------------------}

renderScroll :: forall g m.
     StatefulGen g m
  => g -> RenderCtxt -> Scroll -> m Html
renderScroll gen = go
  where
    go :: RenderCtxt -> Scroll -> m Html
    go ctxt = \case
        Text t ->
          renderText gen ctxt t
        Div id ks ->
          fmap (\elems -> div ! A.id (fromString id) $ sequence_ elems) $
            mapM (go ctxt) ks
        Elements ks ->
          sequence_ <$> mapM (go ctxt) ks
        SetSize size k ->
          go ctxt{renderCtxtSize = size} k
        SetLimit limit k ->
          go ctxt{renderCtxtLimit = limit} k
        SetWiggle wiggle k ->
          go ctxt{renderCtxtWiggle = wiggle} k

renderText :: forall g m.
     StatefulGen g m
  => g -> RenderCtxt -> String -> m Html
renderText gen RenderCtxt{renderCtxtSize = size, ..} =
    go renderCtxtLimit (Pos { posX = 0, posY = 0 })
  where
    go :: [Int] -> Pos -> String -> m Html
    go _     _   []     = return $ return ()
    go limit pos (c:cs) = do
        pos' <- wiggle pos
        let this = do div ! A.class_ "scrollElement"
                          ! A.style (renderAttrs $ attrs pos') $
                        fromString [c]
        (this >>) <$> uncurry go (updatePos limit pos) cs

    wiggle :: Pos -> m Pos
    wiggle (Pos x y) = do
        wx <- uniformRM (negate renderCtxtWiggle, renderCtxtWiggle) gen
        wy <- uniformRM (negate renderCtxtWiggle, renderCtxtWiggle) gen
        return $ Pos (x + wx) (y + wy)

    updatePos :: [Int] -> Pos -> ([Int], Pos)
    updatePos []     (Pos x y) = ([]       , Pos x (y + size))
    updatePos (1:ls) (Pos x _) = (ls       , Pos (x - size) 0)
    updatePos (l:ls) (Pos x y) = ((l-1):ls , Pos x (y + size))

    attrs :: Pos -> [(AttrName, AttrValue)]
    attrs (Pos x y) = [
          ("left"   , show x    `WithUnit` "px")
        , ("top"    , show y    `WithUnit` "px")
        , ("width"  , show size `WithUnit` "px")
        , ("height" , show size `WithUnit` "px")
        ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

data Pos = Pos { posX :: Int, posY :: Int }

type AttrName = String

data AttrValue = Unitless String | WithUnit String String

renderAttrs :: [(AttrName, AttrValue)] -> AttributeValue
renderAttrs = fromString . intercalate " " . map (uncurry aux)
  where
    aux :: AttrName -> AttrValue -> String
    aux prop (Unitless value)        = prop ++ ": " ++ value ++ ";"
    aux prop (value `WithUnit` unit) = prop ++ ": " ++ value ++ unit ++ ";"
