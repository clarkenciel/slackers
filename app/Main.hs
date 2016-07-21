{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Web.Spock.Safe
import Control.Monad.Trans (liftIO)

main :: IO ()
main = 
  runSpock 8080 $ spockT id $
    get "vimgif" $ liftIO serveRandomVimGif >>= json
