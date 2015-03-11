{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative           ((<$>))
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Loops           (dropWhileM)
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
tryGoingTo s' url =
    let withShortState = (liftIO .) . withMVar

    in do originalURL <- withShortState s' $ \s -> lookup (storage s) url
          case originalURL of
              Nothing             -> cantResolveURL url
              Just (URL endpoint) -> redirect endpoint


shorten :: MVar ShortState -> OriginalURL -> ActionM ()
shorten s' url = do
    su <- liftIO $ generateShortenedURL s' url
    case su of
        Nothing  -> couldntCreateURL url
        Just su' -> createdURL url su'


generateShortenedURL :: MVar ShortState -> OriginalURL -> IO (Maybe ShortenedURL)
generateShortenedURL s' url =
    maybeModifyMVar s' $ \s ->
        runMaybeT $ do
            (shortURL:remaining) <- lift $ dropWhileM (member $ storage s) (urls s)
            _ <- lift $ insert (storage s) shortURL url
            return (s{urls = remaining}, shortURL)


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

