-- | Module running the actual web server
--
{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Server
    ( main
    ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import System.Environment (getArgs)
import Data.Monoid (mappend)

import Snap.Types
import Snap.Http.Server (httpServe)
import Snap.Util.FileServe (fileServe)

import OmNomNom.Database
import OmNomNom.Types
import OmNomNom.Templates (blaze)
import qualified OmNomNom.Templates as Templates
import OmNomNom.Users (getUserFromCookie)
import qualified OmNomNom.Users as Users (routes)
import OmNomNom.Shop (_shop, _cart)
import qualified OmNomNom.Shop as Shop (routes)

-- | Site root
--
root :: Snap ()
root = do
    muser <- getUserFromCookie
    case muser of
        Left _ -> redirect "/login"
        Right _ -> do
            inner <- mappend <$> _shop <*> _cart
            blaze $ Templates.root inner

-- | Main site handler
--
site :: Snap ()
site = fileServe "static" <|> route routes
  where
    routes = concat
        [ [("", ifTop root)]
        , Users.routes
        , Shop.routes
        ]

-- | Main function
--
main :: IO ()
main = do
    -- Set some items
    withRedis $ \redis -> mapM_ (setAdd redis "products" . Product)
        [ "A large sandwich"
        , "A small sandwich"
        , "Home-made cookies"
        ]

    -- Read the port to run on
    port <- getArgs >>= \args -> return $ case args of
        [p] -> read p
        _   -> 8000

    -- Start site using the main handler
    httpServe "*" port "omnomnom" (Just "access.log") (Just "error.log") site
