{-# LANGUAGE OverloadedStrings #-}

{- VimGif will provide a random gif from vimgif.com
   This works fine, but could really benefit from using MaybeT.
   Oh well, someday!
-}

module VimGif (serveRandomVimGif) where


import qualified Text.XML as X
import qualified Text.HTML.DOM as D
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Simple as S
import qualified Control.Monad.Random as R
import qualified Data.Aeson as A
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, splitOn, append, unpack)
import Data.String.Conversions (cs)
import Prelude hiding (lookup)
import Data.Map.Strict (lookup)

{- "MAIN" function -}

--serveRandomVimGif :: IO 
serveRandomVimGif = do
  pageLinks <- getVimGifPageLinks
  case pageLinks of
    Nothing -> return ["No gifs found"]
    Just pls -> do
      choice <- fst . R.randomR (0, length pls - 1) <$> R.newStdGen
      gifPage <- S.parseRequest $ unpack (pls !! choice)
      gifs <- getGifsFrom gifPage
      case gifs of
        Nothing -> return ["No gifs found"]
        Just gs -> return gs


{- RESPONSE PARSING -}
safeHead l 
  | null l = Nothing 
  | otherwise = Just $ head l

matchesName t = (== t) . X.nameLocalName . X.elementName

getElements :: [X.Node] -> [X.Element]
getElements = map extract . filter isNodeElement
  where extract (X.NodeElement e) = e
        isNodeElement (X.NodeElement e) = True
        isNodeElement _ = False

getChildren :: X.Element -> [X.Element]
getChildren = getElements . X.elementNodes

getBody :: X.Document -> Maybe X.Element
getBody = safeHead . bodyElements . getChildren . X.documentRoot
  where bodyElements = filter isBody
        isBody = (== "body") . X.nameLocalName . X.elementName

getTagsByName :: Text -> X.Element -> [X.Element]
getTagsByName tagName = concatMap branch . getChildren
  where branch element = if tagName `matchesName` element
                            then [element]
                            else getTagsByName tagName element


getTagsIn tagName lookupFunc = 
  extractValues . onlySuccess . map lookupFunc . getTagsByName tagName
    where onlySuccess = filter isJust
          extractValues = map fromJust

getAttribute attrName = lookup attrName . X.elementAttributes

getHrefsIn :: X.Element -> [Text]
getHrefsIn = getTagsIn "a" (getAttribute "href")

getImgsIn :: X.Element -> [Text]
getImgsIn = getTagsIn "img" (getAttribute "src")

lbsTo :: (X.Element -> a) -> L.ByteString -> Maybe a
lbsTo f = fmap f . getBody . D.parseLBS

lbsToHrefs :: L.ByteString -> Maybe [Text]
lbsToHrefs = lbsTo getHrefsIn

lbsToImg :: L.ByteString -> Maybe [Text]
lbsToImg = lbsTo getImgsIn

getGifLinks :: [Text] -> [Text]
getGifLinks = filter (\h -> includesGif h && isHtml h)
  where includesGif = ("_gifs" `elem`) . splitOn "/"
        isHtml = (== "html") . htmlPart
        htmlPart = last . splitOn "."

getGifImgs :: [Text] -> [Text]
getGifImgs = filter isGif
  where isGif = ("gif" ==) . last . splitOn "."

getGifPageLinks :: [Text] -> [Text]
getGifPageLinks = map buildURL . getGifLinks
  where buildURL stem =  "http://vimgifs.com/" `append` filename stem `append` "/"
        filename = last . splitOn "/" . head . splitOn "."

processBodyWith :: (L.ByteString -> a) -> S.Request -> IO a
processBodyWith processor url = processor . S.getResponseBody <$> S.httpLbs url

-- note the two fmaps here to deal with the Maybe-inside-IO thing
getVimGifPageLinks :: IO (Maybe [Text])
getVimGifPageLinks =  processBodyWith (fmap getGifPageLinks . lbsToHrefs) "https://github.com/mrmrs/vimgifs/tree/gh-pages/_gifs"

getGifsFrom :: S.Request -> IO (Maybe [Text])
getGifsFrom = processBodyWith (fmap getGifImgs . lbsToImg)
