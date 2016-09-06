{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module VimGif (serveRandomVimGif) where

import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Simple as S
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import Control.Monad.Random (MonadRandom, getRandomR)
import GHC.Generics (Generic)
import Data.Vector (Vector(..), (!), fromList)
import Data.Aeson 

-- | wrapping Text in a newtype
-- this will get us the encoding/decoding JSON features
-- and the wrapper disappears at runtime
newtype GifTitle = GifTitle { t :: Text }
  deriving (Show, Generic)

instance ToJSON GifTitle
instance FromJSON GifTitle where
  parseJSON (String t) = pure $ GifTitle t
  parseJSON (Number n) = pure (GifTitle $ pack (show n))
  parseJSON _ = mempty

data Gif
  = Gif
  { title :: GifTitle
  , url :: Text
  , awslink :: Text
  } deriving (Show, Generic)

instance FromJSON Gif
instance ToJSON Gif

type GifList = Vector Gif

serveRandomVimGif :: IO Value
serveRandomVimGif = gifToSlack <$> (getGifs >>= chooseRandom)

getGifs :: IO GifList
getGifs = fromMaybe (fromList [Gif (GifTitle "") "" ""]) . decode . S.getResponseBody 
          <$> S.httpLbs "https://vimgifs.com/gifs.json"

chooseRandom :: (MonadRandom m) => Vector a -> m a
chooseRandom gifs = do
  idx <- getRandomR (0, length gifs - 1)
  return $ gifs ! idx

gifToSlack :: Gif -> Value
gifToSlack Gif{..} = object [ "attachments" .= [ object [ "text" .= t title
                                                       , "image_url" .= awslink ] ] ]
