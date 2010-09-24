-- | Module running the actual web server
--
{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Server
    ( -- * Stuff
    ) where

import Control.Applicative ((<|>))
import System.Environment (getArgs)

import Snap.Types
import Snap.Http.Server (httpServe)
import Snap.Util.FileServe (fileServe)

import OmNomNom.Templates (blaze)
import qualified OmNomNom.Templates as Templates

-- | Main site handler
--
site :: Snap ()
site = fileServe "static" <|> route
    [ ("", ifTop $ blaze Templates.root)
    ]

-- | Main function
--
main :: IO ()
main = do
    -- Read the port to run on
    port <- getArgs >>= \args -> return $ case args of
        [p] -> read p
        _   -> 8000

    -- Start site using the main handler
    httpServe "*" port "omnomnom" Nothing Nothing site
