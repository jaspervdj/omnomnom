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
        Nothing -> users
        Just _ -> do shop
                     cart

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
                         itemSet redis (userKey name) (User name [])
    
    -- Show the user list
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
    root
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
    , ("/add-user", addUser)
    , ("/login", login)
    , ("/logout", logout)
    , ("/order", order)
    , ("/cart", cart)
    ]

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
