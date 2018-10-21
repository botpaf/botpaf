{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Data.Text (Text)
-- import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Concurrent.Async ( Async, async )
import Control.Concurrent.STM ( atomically, TQueue, newTQueueIO, readTQueue )

import Bot.Config

import Twitch.IRC

data Bot = Bot
  { ircMessages :: TQueue RawIrcMsg
  , ircAsync :: Maybe (Async ())
  }

main :: IO ()
main = do
  config <- getBotConfig "botpaf.ini"
  withConnection config $ \h -> do
    tchan <- newTQueueIO
    registerAndJoin config h
    thread <- async $ collectMsgs config h tchan
    main' Bot
      { ircMessages = tchan
      , ircAsync = Just thread }

getEvent :: Bot -> IO RawIrcMsg
getEvent = atomically . readTQueue . ircMessages

main' :: Bot -> IO ()
main' bot = do
  ev <- getEvent bot
  case ev of
    _ -> doEchoRawMsg bot ev >>= main'

doEchoRawMsg :: Bot -> RawIrcMsg -> IO Bot
doEchoRawMsg bot msg = do
  Text.putStr $ "BOT: " <> renderMsg msg
  return bot
