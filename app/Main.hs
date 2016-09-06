{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (lines, lookup, readFile)
import Lib
import Web.Spock.Safe
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (liftIO)
import System.Environment (lookupEnv)
import Data.Text (Text(..), splitOn, lines)
import Data.Text.IO (readFile)
import Data.Map as M (Map(..), lookup, fromList, keysSet)
import Data.Set as S ((\\), fromList, toList)

main :: IO ()
main = do
  port <- getPortNumber
  safeTokens <- getSafeTokens
  let missingTokens = appList \\ keysSet safeTokens
   in if not (null missingTokens)
         then missingTokensError missingTokens
         else runSpock port $ spockT id (slackersAPI safeTokens)

getPortNumber = do
  port <- fmap read <$> lookupEnv "SLACKERS_PORT"
  return $ fromMaybe 8080 port

appList = S.fromList ["vimgif"]

getSafeTokens = M.fromList . map eqToPair . lines <$> readFile "slackers.cfg"
    where eqToPair = (\(x:y:_) -> (x,y)) . splitOn "="

slackersAPI tokenCollection = do
  get "slackers" $ text "hello!"
  get "slackers/vimgif" (liftIO serveRandomVimGif >>= json)

withTokens tokenCollection tokenKey action token =
  if token == lookup tokenKey tokenCollection
     then action
     else text "nope!"

missingTokensError = 
  putStrLn . ("You are missing the following tokens from slackers.cfg, please provide them: " ++) . show . toList

