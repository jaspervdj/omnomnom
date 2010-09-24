-- | Module running the actual web server
--
{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Server
    ( -- * Stuff
    ) where

import Data.List (sort)
import Control.Applicative ((<|>), (<$>))
import System.Environment (getArgs)
import Control.Monad.Trans (liftIO)
import Control.Monad (forM, (=<<))
import Data.Maybe (catMaybes)

import Snap.Types
import Snap.Http.Server (httpServe)
import Snap.Util.FileServe (fileServe)
import qualified Data.ByteString as SB
import qualified Codec.Binary.UTF8.String as Utf8 (decode, encode)

import OmNomNom.Database
import OmNomNom.Types
import OmNomNom.Templates (blaze)
import qualified OmNomNom.Templates as Templates

-- | Site root
--
root :: Snap ()
root = blaze Templates.root

-- | Site header
--
header :: Snap ()
header = blaze . Templates.header =<< getUserFromCookie

-- | User list
--
users :: Snap ()
users = withRedis $ \redis -> do
    keys <- liftIO $ setFindAll redis "users"
    users' <- liftIO $ fmap catMaybes $ forM keys $ itemGet redis
    blaze $ Templates.users $ sort users'

-- | Add a user
--
addUser :: Snap ()
addUser = withRedis $ \redis -> do
    params <- rqParams <$> getRequest
    Just name <- fmap (Utf8.decode . SB.unpack) <$> getParam "name"
    exists <- liftIO $ setContains redis "users" $ userKey name
    if exists || null name
        then blaze $ Templates.warning "Invalid username."
        else liftIO $ do setAdd redis "users" $ userKey name
                         itemSet redis (userKey name) (User name)
    users

-- | Log in
--
login :: Snap ()
login = do
    Just name <- fmap (Utf8.decode . SB.unpack) <$> getParam "name"
    modifyResponse $ addCookie $ cookie name
    root
  where
    cookie name = Cookie
        { cookieName = "name"
        , cookieValue = SB.pack $ Utf8.encode name
        , cookieExpires = Nothing
        , cookieDomain = Nothing
        , cookiePath = Just "/"
        }

-- | Main site handler
--
site :: Snap ()
site = fileServe "static" <|> route
    [ ("", ifTop root)
    , ("/header", header)
    , ("/users", users)
    , ("/add-user", addUser)
    , ("/login", login)
    ]

-- | Get the current user (based on cookies)
getUserFromCookie :: Snap (Maybe User)
getUserFromCookie = do
    cookies <- rqCookies <$> getRequest
    case filter ((== "name") . cookieName) cookies of
        [] -> return Nothing
        (cookie : _) -> liftIO $ withRedis $ \redis -> itemGet redis $
            userKey $ Utf8.decode $ SB.unpack $ cookieValue cookie

-- | Main function
--
main :: IO ()
main = do
    -- Read the port to run on
    port <- getArgs >>= \args -> return $ case args of
        [p] -> read p
        _   -> 8000

    -- Start site using the main handler
    httpServe "*" port "omnomnom" (Just "access.log") (Just "error.log") site
