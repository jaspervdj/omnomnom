-- | Module exporting the basic types used in omnomnom
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OmNomNom.Types
    ( -- * Basic types
      User (..)
    , userKey
    , Product (..)
    , productCount
    ) where

import Control.Arrow ((&&&))
import Data.List (group, sort)
import Control.Applicative ((<$>), (<*>))
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Codec.Binary.UTF8.String as Utf8 (encode)

import OmNomNom.Database

data User = User
    { userEmail    :: String
    , userProducts :: [Product]
    , userPassword :: ByteString
    } deriving (Show, Ord, Eq)

instance Binary User where
    get = User <$> get <*> get <*> get
    put (User n p d) = put n >> put p >> put d

userKey :: String -> Key
userKey email = Key $ SB.pack $ Utf8.encode $ "user-" ++ email

newtype Product = Product {unProduct :: String}
                deriving (Show, Binary, Ord, Eq)

-- | Create a frequency list from a list of products
--
productCount :: [Product] -> [(Product, Int)]
productCount = map (head &&& length) . group . sort
