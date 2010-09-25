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
import Control.Monad (forM, (=<<), mapM_, when)
import Data.Maybe (catMaybes)

import Snap.Types
import Snap.Http.Server (httpServe)
import Snap.Util.FileServe (fileServe)
import qualified Data.ByteString as SB
import qualified Codec.Binary.UTF8.String as Utf8 (decode, encode)
import Text.Blaze (string)

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
header = blaze Templates.header

-- | Content section
--
content :: Snap ()
content = do
    muser <- getUserFromCookie
    case muser of
        Nothing -> login
        Just _ -> do shop
                     cart

-- | Log in
--
login :: Snap ()
login = do
    mname <- fmap (Utf8.decode . SB.unpack) <$> getParam "login-form-name"
    mpw <- fmap (Utf8.decode . SB.unpack) <$> getParam "login-form-password"
    muser <- addUser mname mpw
    case muser of
        Just user -> modifyResponse $ addCookie (cookie $ userName user)
                                    . setHeader "Location" "/"
                                    . setResponseCode 302
        Nothing -> blaze Templates.login
  where
    cookie name = Cookie
        { cookieName = "user"
        , cookieValue = SB.pack $ Utf8.encode name
        , cookieExpires = Nothing
        , cookieDomain = Nothing
        , cookiePath = Just "/"
        }

-- | Log out
--
logout :: Snap ()
logout = do
    modifyResponse $ addCookie cookie
                   . setHeader "Location" "/"
                   . setResponseCode 302
  where
    cookie = Cookie
        { cookieName = "user"
        , cookieValue = ""
        , cookieExpires = Nothing
        , cookieDomain = Nothing
        , cookiePath = Just "/"
        }

-- | Show all products
--
shop :: Snap ()
shop = do
    products <- liftIO $ withRedis $ flip setFindAll "products"
    blaze $ Templates.shop $ sort products

-- | Order a product
--
order :: Snap ()
order = withRedis $ \redis-> do
    Just user <- getUserFromCookie
    Just product <- fmap (Product . Utf8.decode . SB.unpack) <$> getParam "name"
    valid <- liftIO $ setContains redis "products" product
    when valid $ do
        let user' = user {userProducts = product : userProducts user}
        liftIO $ itemSet redis (userKey $ userName user) user'

    -- Show the cart
    cart

-- | Show the cart
--
cart :: Snap ()
cart = withRedis $ \redis -> do
    Just user <- getUserFromCookie
    blaze $ Templates.cart user 

-- | Main site handler
--
site :: Snap ()
site = fileServe "static" <|> route
    [ ("", ifTop root)
    , ("/header", header)
    , ("/content", content)
    , ("/login", login)
    , ("/logout", logout)
    , ("/order", order)
    , ("/cart", cart)
    ]

-- | Add a user
--
addUser :: Maybe String       -- ^ Username
        -> Maybe String       -- ^ Password
        -> Snap (Maybe User)  -- ^ Resulting user
addUser (Just name) (Just password) = withRedis $ \redis -> do
    exists <- liftIO $ setContains redis "users" $ userKey name
    if exists
        then liftIO $ itemGet redis (userKey name)
        else liftIO $ do
            let user = User name []
            setAdd redis "users" $ userKey name
            itemSet redis (userKey name) user
            return $ Just user
addUser _ _ = return Nothing

-- | Get the current user (based on cookies)
--
getUserFromCookie :: Snap (Maybe User)
getUserFromCookie = do
    cookies <- rqCookies <$> getRequest
    case filter ((== "user") . cookieName) cookies of
        [] -> return Nothing
        (cookie : _) -> liftIO $ withRedis $ \redis -> itemGet redis $
            userKey $ Utf8.decode $ SB.unpack $ cookieValue cookie

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
