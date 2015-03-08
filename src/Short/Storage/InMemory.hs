{-# LANGUAGE NamedFieldPuns #-}

module Short.Storage.InMemory
    (
      mkStorageService
    ) where

import           Control.Applicative    ((<$>))
import           Control.Concurrent.STM
import qualified Data.Map.Lazy          as M

import           Prelude                hiding (lookup)
import           Short.State


mkStorageService :: IO (StorageService)
mkStorageService = atomically $ do
    db <- newTVar M.empty
    let lookup su    = M.lookup su <$> readTVarIO db
        insert su ou = atomically $ do
                           modifyTVar db $ M.insert su ou
                           return $ Just su
        member su    = M.member su <$> readTVarIO db
    return $ StorageService{lookup, insert, member}
