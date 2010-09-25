{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Util
    ( makeSimpleCookie
    , partial
    ) where

import Snap.Types
import Data.ByteString (ByteString)
import Text.Blaze (Html)

import OmNomNom.Templates (blaze)

makeSimpleCookie :: ByteString  -- ^ Name
                 -> ByteString  -- ^ Value
                 -> Cookie      -- ^ Cookie
makeSimpleCookie name value = Cookie
    { cookieName = name
    , cookieValue = value
    , cookieExpires = Nothing
    , cookieDomain = Nothing
    , cookiePath = Just "/"
    }

-- | Render a partial
--
partial :: Snap Html -> Snap ()
partial = (>>= blaze)
