{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Web.Spock.Safe
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (liftIO)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  port <- getPortNumber
  safeToken <- getSafeToken
  runSpock port $ spockT id $ do
    get "slackers" $ text "hello!"
    get "slackers/vimgif" <//> var $ (\token -> 
        if token == safeToken
          then liftIO serveRandomVimGif >>= json
          else text "nope!"

getPortNumber = do
  -- once again, a double fmap since lookupEnv :: IO (Maybe String)
  -- so, <$> gets (fmap read) into IO and fmap gets read into Maybe.
  -- so, this gives us an IO (Maybe Int) (technically Read a => IO (Maybe a))
  port <- fmap read <$> lookupEnv "SLACKERS_PORT"
  return $ fromMaybe 8080 port

getSafeToken = do
  
