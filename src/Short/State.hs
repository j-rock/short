module Short.State where

import           Control.Applicative     ((<$>))
import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Monad           (replicateM)
import           Control.Monad.Random
import qualified Data.Text.Lazy          as T

newtype URL a = URL T.Text deriving (Eq, Ord)

data Original  = Original
data Shortened = Shortened


type OriginalURL  = URL Original
type ShortenedURL = URL Shortened


data StorageService = StorageService
    { lookup :: ShortenedURL -> IO (Maybe OriginalURL)
    , insert :: ShortenedURL -> OriginalURL -> IO (Maybe ShortenedURL)
    , member :: ShortenedURL -> IO Bool
    }

data ShortState = ShortState
    { storage :: StorageService
    , urls    :: [ShortenedURL]
    }


mkShortState :: IO StorageService -> IO (MVar ShortState)
mkShortState mkStorageService =
    do store <- mkStorageService
       randURLs <- evalRandIO randomURLs
       newMVar ShortState {storage = store, urls = randURLs}

randomURLs :: RandomGen g => Rand g [ShortenedURL]
randomURLs =
    let allChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

        randomURL :: RandomGen g => Rand g ShortenedURL
        randomURL = URL . T.pack <$> do
                        nChars <- getRandomR (4, 10)
                        replicateM nChars $ uniform allChars

    in sequence $ repeat randomURL
