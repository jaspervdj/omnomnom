{-# LANGUAGE OverloadedStrings #-}
module OmNomNom.Shop
    ( routes
    , _shop
    , _cart
    ) where

import Data.List (sort)
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)

import Snap.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Codec.Binary.UTF8.String as Utf8 (decode)
import Text.Blaze (Html)

import OmNomNom.Database
import OmNomNom.Types
import OmNomNom.Util
import qualified OmNomNom.Templates as Templates
import OmNomNom.Users (getUserFromCookie)

-- | Module routes
--
routes :: [(ByteString, Snap ())]
routes = [ ("/_shop", partial _shop)
         , ("/_cart", partial _cart)
         ]

-- | Show all products
--
_shop :: Snap Html
_shop = do
    products <- liftIO $ withRedis $ flip setFindAll "products"
    return $ Templates._shop $ sort products

-- | Show the cart (order a product if it's a POST)
--
_cart :: Snap Html
_cart = do
    Right user <- getUserFromCookie
    mproduct <- fmap (Product . Utf8.decode . SB.unpack) <$> getParam "name"
    user' <- case mproduct of Just p   -> addUserProduct user p
                              Nothing  -> return user
    return $ Templates._cart user'

-- | Add a product to a user
--
addUserProduct :: User       -- ^ User
               -> Product    -- ^ Product name
               -> Snap User  -- ^ Resulting user
addUserProduct user product' = withRedis $ \redis -> do
    valid <- liftIO $ setContains redis "products" product'
    if valid
        then do
            let user' = user {userProducts = product' : userProducts user}
            liftIO $ itemSet redis (userKey $ userEmail user) user'
            return user'
        else return user
