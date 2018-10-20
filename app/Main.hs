{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Bot.IRC.Config

main :: IO ()
main = do
  config <- parseConfig "botpaf.ini"
  print config
