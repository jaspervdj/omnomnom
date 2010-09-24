-- | Module providing different templates for the site
--
{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Templates
    ( blaze
    , root
    ) where

import Snap.Types
import Text.Blaze (Html)
import Text.Blaze.Renderer.Utf8 (renderHtml)

-- | Send a blazehtml response to the browser
--
blaze :: Html -> Snap ()
blaze html = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml html

-- | Main template
--
root :: Html
root = "Hello world!"
