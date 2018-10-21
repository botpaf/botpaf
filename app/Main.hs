{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Data.Foldable ( for_ )

-- import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- import Control.Monad ( when )

import Control.Concurrent.Async ( Async, async )
import Control.Concurrent.STM ( atomically, TQueue, newTQueueIO, readTQueue )

import Bot.Config

import Twitch.IRC

data Bot
  = Bot
    { ircMessages :: TQueue RawIrcMsg
    , ircAsync :: Maybe (Async ())
    }

main :: IO ()
main = do
  config <- getBotConfig "botpaf.ini"
  tchan  <- newTQueueIO
  thread <- async $ connect config tchan
  main' Bot
    { ircMessages = tchan , ircAsync = Just thread }

getEvent :: Bot -> IO RawIrcMsg
getEvent = atomically . readTQueue . ircMessages

main' :: Bot -> IO ()
main' bot = do
  ev <- getEvent bot
  case ev ^. msgCommand of
    "PRIVMSG" -> resume =<< doMsg bot ev
    _         -> main' =<< doEchoRawMsg bot ev
  where
    resume Nothing     = pure ()
    resume (Just bot') = main' bot'

doEchoRawMsg :: Bot -> RawIrcMsg -> IO Bot
doEchoRawMsg bot msg = do
  Text.putStr $ "BOT: " <> renderMsg msg
  return bot

doMsg :: Bot -> RawIrcMsg -> IO (Maybe Bot)
doMsg bot msg = do
  let [room,text] = msg ^. msgParams
  if ("!quit" `Text.isPrefixOf` text)
  then do
    Text.putStrLn $ "received quit signal in " <> room
    return Nothing
  else
    return $ Just bot
