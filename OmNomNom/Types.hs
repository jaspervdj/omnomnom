-- | Module exporting the basic types used in omnomnom
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OmNomNom.Types
    ( -- * Basic types
      User (..)
    , userKey
    ) where

import Data.Binary (Binary)
import qualified Data.ByteString as SB
import qualified Codec.Binary.UTF8.String as Utf8 (encode)

import OmNomNom.Database

newtype User = User {unUser :: String}
             deriving (Show, Binary, Ord, Eq)

userKey :: String -> Key
userKey name = Key $ SB.pack $ Utf8.encode $ "user-" ++ name
