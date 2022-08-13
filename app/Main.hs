module Main where

import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes qualified as A

{-------------------------------------------------------------------------------
  HTML rendering
-------------------------------------------------------------------------------}

page :: Html
page = docTypeHtml $
    return ()

{-------------------------------------------------------------------------------
  Application driver
-------------------------------------------------------------------------------}

main :: IO ()
main = putStrLn $ renderHtml page
