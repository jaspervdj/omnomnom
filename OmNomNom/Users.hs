-- | Module dealing with user management
--
{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Users
    ( routes
    , getUserFromCookie
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)

import Snap.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Codec.Binary.UTF8.String as Utf8 (decode, encode)
import Data.Digest.Pure.SHA (bytestringDigest, sha1)

import OmNomNom.Database
import OmNomNom.Types
import OmNomNom.Util
import OmNomNom.Templates (blaze)
import qualified OmNomNom.Templates as Templates

-- | Module routes
--
routes :: [(ByteString, Snap ())]
routes = [ ("/login", login)
         , ("/logout", logout)
         ]

-- | Log in
--
login :: Snap ()
login = do
    memail <- fmap (Utf8.decode . SB.unpack) <$> getParam "login-form-email"
    mpw <- fmap digest <$> getParam "login-form-password"
    muser <- fromMaybe (return $ Left "Please log in") $
        addUser <$> memail <*> mpw
    case muser of
        Right user -> do
            modifyResponse $ addCookie (nameCookie $ userEmail user)
                           . addCookie (passwordCookie $ userPassword user)
            redirect "/"
        Left e -> blaze $ Templates.login e
  where
    nameCookie = makeSimpleCookie "user-email" . SB.pack . Utf8.encode
    passwordCookie = makeSimpleCookie "user-password"
    digest = SB.concat . LB.toChunks . bytestringDigest
           . sha1 . LB.fromChunks . return

-- | Log out
--
logout :: Snap ()
logout = do modifyResponse $ addCookie (makeSimpleCookie "user-email" "")
                           . addCookie (makeSimpleCookie "user-password" "false")
            redirect "/"

-- | Add a user
--
addUser :: String                     -- ^ Email
        -> ByteString                 -- ^ Password hash
        -> Snap (Either String User)  -- ^ Resulting user
addUser email password = withRedis $ \redis -> do
    exists <- liftIO $ setContains redis "users" $ userKey email
    if exists
        then liftIO $ do
            Just user <- itemGet redis (userKey email)
            return $ if userPassword user == password
                            then Right user
                            else Left "Wrong password"
        else liftIO $ do
            let user = User email [] password
            setAdd redis "users" $ userKey email
            itemSet redis (userKey email) user
            return $ Right user

-- | Get the current user (based on cookies)
--
getUserFromCookie :: Snap (Either String User)
getUserFromCookie = do
    cookies <- rqCookies <$> getRequest
    let n = filter ((== "user-email") . cookieName) cookies
        p = filter ((== "user-password") . cookieName) cookies
    case (n, p) of
        ([email], [password]) -> do
            u <- liftIO $ withRedis $ \redis -> itemGet redis $ userKey
                        $ Utf8.decode $ SB.unpack $ cookieValue email
            case u of Just user -> if userPassword user == cookieValue password
                                        then return $ Right user
                                        else return $ Left $ show (userPassword user, cookieValue password)
                      _ -> return $ Left "User not found"
        _ -> return $ Left "Wrong parameters"
