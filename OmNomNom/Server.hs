-- | Module running the actual web server
--
{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Server
    ( -- * Stuff
    ) where

import Data.List (sort)
import Control.Applicative ((<|>), (<$>), (<*>))
import System.Environment (getArgs)
import Control.Monad.Trans (liftIO)
import Control.Monad (forM, (=<<), mapM_, when)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Monoid (mappend)

import Snap.Types
import Snap.Http.Server (httpServe)
import Snap.Util.FileServe (fileServe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Codec.Binary.UTF8.String as Utf8 (decode, encode)
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Text.Blaze (Html)

import OmNomNom.Database
import OmNomNom.Types
import OmNomNom.Templates (blaze)
import qualified OmNomNom.Templates as Templates

-- | Site root
--
root :: Snap ()
root = do
    muser <- getUserFromCookie
    case muser of
        Left e -> do
            liftIO $ putStrLn e
            redirect "/login"
        Right _ -> do
            inner <- mappend <$> _shop <*> _cart
            blaze $ Templates.root inner

-- | Log in
--
login :: Snap ()
login = do
    mname <- fmap (Utf8.decode . SB.unpack) <$> getParam "login-form-name"
    mpw <- fmap digest <$> getParam "login-form-password"
    muser <- fromMaybe (return $ Left "Please log in") $
        addUser <$> mname <*> mpw
    case muser of
        Right user ->
            modifyResponse $ addCookie (nameCookie $ userName user)
                           . addCookie (passwordCookie $ userPassword user)
                           . setHeader "Location" "/"
                           . setResponseCode 302
        Left e -> blaze $ Templates.login e
  where
    nameCookie = makeSimpleCookie "user-name" . SB.pack . Utf8.encode
    passwordCookie = makeSimpleCookie "user-password"
    digest = SB.concat . LB.toChunks . bytestringDigest
           . sha1 . LB.fromChunks . return

-- | Log out
--
logout :: Snap ()
logout = modifyResponse $ addCookie (makeSimpleCookie "user-name" "")
                        . addCookie (makeSimpleCookie "user-password" "false")
                        . setHeader "Location" "/"
                        . setResponseCode 302

-- | Render a partial
--
partial :: Snap Html -> Snap ()
partial = (>>= blaze)

-- | Show all products
--
_shop :: Snap Html
_shop = do
    products <- liftIO $ withRedis $ flip setFindAll "products"
    return $ Templates._shop $ sort products

-- | Show the cart (order a product if it's a POST)
--
_cart :: Snap Html
_cart = withRedis $ \redis -> do
    Right user <- getUserFromCookie
    mproduct <- fmap (Product . Utf8.decode . SB.unpack) <$> getParam "name"
    user' <- case mproduct of Just product -> addUserProduct user product
                              Nothing      -> return user
    return $ Templates._cart user'

-- | Main site handler
--
site :: Snap ()
site = fileServe "static" <|> route
    [ ("", ifTop root)
    , ("/login", login)
    , ("/logout", logout)
    , ("/_shop", partial _shop)
    , ("/_cart", partial _cart)
    ]

-- | Add a user
--
addUser :: String                     -- ^ Username
        -> ByteString                 -- ^ Password hash
        -> Snap (Either String User)  -- ^ Resulting user
addUser name password = withRedis $ \redis -> do
    exists <- liftIO $ setContains redis "users" $ userKey name
    if exists
        then liftIO $ do
            Just user <- itemGet redis (userKey name)
            return $ if userPassword user == password
                            then Right user
                            else Left "Wrong password"
        else liftIO $ do
            let user = User name [] password
            setAdd redis "users" $ userKey name
            itemSet redis (userKey name) user
            return $ Right user

-- | Get the current user (based on cookies)
--
getUserFromCookie :: Snap (Either String User)
getUserFromCookie = do
    cookies <- rqCookies <$> getRequest
    let n = filter ((== "user-name") . cookieName) cookies
        p = filter ((== "user-password") . cookieName) cookies
    case (n, p) of
        ([name], [password]) -> do
            u <- liftIO $ withRedis $ \redis -> itemGet redis $ userKey
                        $ Utf8.decode $ SB.unpack $ cookieValue name
            case u of Just user -> if userPassword user == cookieValue password
                                        then return $ Right user
                                        else return $ Left $ show (userPassword user, cookieValue password)
                      _ -> return $ Left "User not found"
        _ -> return $ Left "Wrong parameters"

-- | Add a product to a user
--
addUserProduct :: User       -- ^ User
               -> Product    -- ^ Product name
               -> Snap User  -- ^ Resulting user
addUserProduct user product = withRedis $ \redis -> do
    valid <- liftIO $ setContains redis "products" product
    if valid
        then do
            let user' = user {userProducts = product : userProducts user}
            liftIO $ itemSet redis (userKey $ userName user) user'
            return user'
        else return user

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
