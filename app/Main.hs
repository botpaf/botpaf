{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Bot.IRC.Config
import Bot.IRC

main :: IO ()
main = do
  config <- parseConfig "botpaf.ini"
  withConnection config $ \h -> do
    sendHello config h
    eventLoop config h
