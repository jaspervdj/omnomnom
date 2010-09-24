-- | Module providing database functions at a higher level
--
module OmNomNom.Database
    ( withRedis
    , module Database.Redis.Simple
    ) where

import Control.Monad.IO.Class

import Database.Redis.Redis (Redis, connect, localhost, defaultPort, disconnect)
import Database.Redis.Simple

withRedis :: MonadIO m => (Redis -> m a) -> m a
withRedis f = do
    redis <- liftIO $ connect localhost defaultPort
    result <- f redis
    liftIO $ disconnect redis
    return result
