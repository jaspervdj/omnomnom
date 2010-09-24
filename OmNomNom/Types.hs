-- | Module exporting the basic types used in omnomnom
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OmNomNom.Types
    ( -- * Basic types
      User (..)
    , userKey
    , Product (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary
import qualified Data.ByteString as SB
import qualified Codec.Binary.UTF8.String as Utf8 (encode)

import OmNomNom.Database

data User = User
    { userName :: String
    , userProducts :: [Product]
    } deriving (Show, Ord, Eq)

instance Binary User where
    get = User <$> get <*> get
    put (User n p) = put n >> put p

userKey :: String -> Key
userKey name = Key $ SB.pack $ Utf8.encode $ "user-" ++ name

newtype Product = Product {unProduct :: String}
                deriving (Show, Binary, Ord, Eq)
