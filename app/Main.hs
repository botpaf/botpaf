{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Bot.Config

import Twitch.IRC

main :: IO ()
main = do
  config <- getBotConfig "botpaf.ini"
  withConnection config $ \h -> do
    sendHello config h
    sendJoin config h
    eventLoop config h
