{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (lines, lookup, readFile)
import Lib
import Web.Spock.Safe
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (liftIO)
import System.Environment (lookupEnv)
import Data.Text (splitOn, lines)
import Data.Text.IO (readFile)
import Data.Map as M (lookup, fromList, keysSet)
import Data.Set as S ((\\), fromList, toList)

main :: IO ()
main = do
  port <- getPortNumber
  safeTokens <- getSafeTokens
  let missingTokens = appList \\ keysSet safeTokens
   in if not (null missingTokens)
         then missingTokensError missingTokens
         else
           runSpock port $ spockT id $ do
           get "slackers" $ text "hello!"
           get ("slackers/vimgif" <//> var) (\token -> 
             if token == lookup "vimgif" safeTokens
                then liftIO serveRandomVimGif >>= json
                else text "nope!")
                  where missingTokensError = putStrLn . ("You are missing the following tokens from slackers.cfg, please provide them: " ++) . show . toList

getPortNumber = do
  -- once again, a double fmap since lookupEnv :: IO (Maybe String)
  -- so, <$> gets (fmap read) into IO and fmap gets read into Maybe.
  -- so, this gives us an IO (Maybe Int) (technically Read a => IO (Maybe a))
  port <- fmap read <$> lookupEnv "SLACKERS_PORT"
  return $ fromMaybe 8080 port

appList = S.fromList ["vimgif"]

getSafeTokens = M.fromList . map eqToPair . lines <$> readFile "slackers.cfg"
    where eqToPair = (\(x:y:_) -> (x,y)) . splitOn "="
