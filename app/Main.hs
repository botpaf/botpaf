{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Data.Foldable ( for_ )

-- import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- import Control.Monad ( when )

import Lens.Micro.Platform ( (&), (^..) )

import Control.Concurrent.Async ( Async, async )
import Control.Concurrent.STM ( atomically, TQueue, newTQueueIO, readTQueue )

import Bot.Config

import Twitch.IRCv32

data Bot
  = Bot
    { ircMessages :: TQueue Message
    , ircAsync :: Maybe (Async ())
    }

main :: IO ()
main = do
  config <- getBotConfig "botpaf.ini"
  tchan  <- newTQueueIO
  thread <- async $ connect config tchan
  main' Bot
    { ircMessages = tchan , ircAsync = Just thread }

getEvent :: Bot -> IO Message
getEvent = atomically . readTQueue . ircMessages

main' :: Bot -> IO ()
main' bot = do
  ev <- getEvent bot
  case ev of
    Privmsg _ _ _ _ -> resume =<< doMsg bot ev
    _               ->  main' =<< doEchoRawMsg bot ev
  where
    resume Nothing     = pure ()
    resume (Just bot') = main' bot'

doEchoRawMsg :: Bot -> Message -> IO Bot
doEchoRawMsg bot msg = do
  Text.putStrLn $ "NoParse: " <> (msg & show & Text.pack)
  return bot

doMsg :: Bot -> Message -> IO (Maybe Bot)
doMsg bot msg = do
  let [room,text] = msg ^.. (msgRoom . roomName <> msgText)
  if ("!quit" `Text.isPrefixOf` text)
  then do
    Text.putStrLn $ "received quit signal in " <> room
    return Nothing
  else do
    Text.putStrLn $ "Parsed: " <> (Text.pack $ show msg)
    return $ Just bot
