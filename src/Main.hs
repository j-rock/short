{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative           ((<$>))
import           Control.Lens
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans           (lift)
import           Control.Monad.Trans.Maybe

import           Control.Concurrent.MVar
import           Control.Exception.Base

import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Web.Scotty

import           Short.Pages
import           Short.State
import qualified Short.Storage.InMemory        as Store

import           Prelude                       hiding (lookup)



main :: IO ()
main =
    let readURL = URL <$> param "url"

    in do shortState <- mkShortState Store.mkStorageService
          scotty 5678 $ do
              middleware  $ staticPolicy $ addBase "public"
              get "/"       homePage
              get "/:url" $ tryGoingTo shortState =<< readURL
              post "/s"   $ shorten    shortState =<< readURL
              notFound      uhOh


tryGoingTo :: MVar ShortState -> ShortenedURL -> ActionM ()
tryGoingTo s url =
    let withShortState s' act = liftIO $ withMVar s' act

    in do originalURL <- withShortState s $ \s' -> lookup (s' ^. storage) url
          case originalURL of
              Nothing             -> cantResolveURL url
              Just (URL endpoint) -> redirect endpoint


shorten :: MVar ShortState -> OriginalURL -> ActionM ()
shorten s url = do
    su <- liftIO $ generateShortenedURL s url
    case su of
        Nothing  -> couldntCreateURL url
        Just su' -> createdURL url su'


generateShortenedURL :: MVar ShortState -> OriginalURL -> IO (Maybe ShortenedURL)
generateShortenedURL s url =
    let findNextURL _  []     = fail ""
        findNextURL s' (u:us) = do isMem <- lift $ member (s' ^. storage) u
                                   if isMem
                                       then findNextURL s' us
                                       else return (u, us)
    in maybeModifyMVar s $ \s' ->
           runMaybeT $ do
               (shortURL, remaining) <- findNextURL s' $ s' ^. urls
               _ <- lift $ insert (s' ^. storage) shortURL url
               return (s' & urls .~ remaining, shortURL)


-- Peeked at Control.Concurrent.MVar.modifyMVar source code
-- If the io computation returns Nothing, the MVar is unchanged
maybeModifyMVar :: MVar a -> (a -> IO (Maybe (a,b))) -> IO (Maybe b)
maybeModifyMVar m io =
    mask $ \restore -> do
        a' <- takeMVar m
        mab <- restore (io a' >>= evaluate) `onException` putMVar m a'
        case mab of
            Nothing        -> return Nothing
            Just (a'', b') -> putMVar m a'' >> return (Just b')

