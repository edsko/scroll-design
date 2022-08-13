module ScrollDesign.Render.HTML (renderHtml) where

import Text.Blaze.Html.Renderer.Pretty qualified as Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes qualified as A

renderHtml :: String
renderHtml = Blaze.renderHtml render

{-------------------------------------------------------------------------------
  Renderer proper
-------------------------------------------------------------------------------}

render :: Html
render = docTypeHtml $
    return ()

