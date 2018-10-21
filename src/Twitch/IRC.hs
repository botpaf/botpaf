{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- This module uses irc-core to connect to the irc network
-- Use Twitch.IRCv3

module Twitch.IRC
  ( connect
  , RawIrcMsg
  , msgCommand
  , msgParams
  , renderMsg
  , sendMsg
  ) where

import Data.Foldable    ( for_ )
import Data.Traversable ( for )

import Data.Semigroup   ( Sum(..) )
import Data.List.Extra  ( chunksOf )

import           Data.Text                  ( Text )
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text ( putStrLn )

import Control.Exception

import Control.Concurrent.STM ( atomically, TQueue, writeTQueue )

import           Hookup           hiding ( connect )
import qualified Hookup as Hookup

import Irc.RawIrcMsg ( parseRawIrcMsg, asUtf8, RawIrcMsg, renderRawIrcMsg, msgCommand, msgParams )
import Irc.Commands  ( ircCapReq, ircPass, ircNick, ircPong, ircJoin )

import Bot.Config

connect :: BotConfig
        -> TQueue RawIrcMsg
        -> IO ()
connect config tchan = do
  withConnection config $ \h -> do
    registerAndJoin config h
    collectMsgs config h tchan

withConnection :: BotConfig -> (Connection -> IO a) -> IO a
withConnection config = bracket (Hookup.connect $ mkParams config) close
  where
    mkParams config = ConnectionParams
      { cpHost = Text.unpack $ config ^. ircConfig . ircHost
      , cpPort = fromIntegral $ config ^. ircConfig . ircPort
      , cpTls = Just TlsParams { tpClientCertificate = Nothing
                               , tpClientPrivateKey  = Nothing
                               , tpServerCertificate = Nothing
                               , tpCipherSuite       = "HIGH"
                               , tpInsecure          = False }
      , cpSocks = Nothing
      , cpFamily = defaultFamily
      }

readIrcLine :: Connection -> IO (Maybe RawIrcMsg)
readIrcLine h = do
  mb <- recvLine h 1024 -- RFC 1459 «512 for tags + 512 for standard msg»
  for mb $ \xs -> do
    Text.putStrLn $ "RAW: " <> asUtf8 xs
    case parseRawIrcMsg (asUtf8 xs) of
      Just msg -> return $! msg
      Nothing -> fail "Server sent invalid message"

sendMsg :: BotConfig -> Connection -> RawIrcMsg -> IO ()
sendMsg _ h = do
  -- tickRateLimit $ config ^. ircConfig . ircRate -- TODO add Irc.RateLimit
  send h . renderRawIrcMsg

sendHello :: BotConfig -> Connection -> IO ()
sendHello config h = do
  sendMsg config h (ircCapReq ["twitch.tv/tags"])
  sendMsg config h (ircCapReq ["twitch.tv/membership"])
  sendMsg config h (ircCapReq ["twitch.tv/commands"])
  sendMsg config h (ircPass $ config ^. ircConfig . ircToken)
  sendMsg config h (ircNick $ config ^. ircConfig . ircName)

sendJoin :: BotConfig -> Connection -> IO ()
sendJoin config h = do
  let css = chunksOf (Sum . Text.length)
                     (\l -> getSum l <= 510 - Text.length "JOIN :")
                     ((Text.cons '#') <$> config ^. ircConfig . ircRooms)
  for_ css $ \cs -> do
    sendMsg config h (ircJoin (Text.intercalate "," cs) Nothing)

registerAndJoin :: BotConfig -> Connection -> IO ()
registerAndJoin config h = do
  sendHello config h
  sendJoin config h

renderMsg :: RawIrcMsg -> Text
renderMsg = asUtf8 . renderRawIrcMsg

collectMsgs :: BotConfig -> Connection -> TQueue RawIrcMsg -> IO ()
collectMsgs config h tchan = do
  mb <- readIrcLine h
  for_ mb $ \msg -> do
    -- handle PING/PONG and relay all the rest
    case msg ^. msgCommand of
      "PING" -> sendMsg config h $ ircPong $ msg ^. msgParams
      _      -> atomically $ writeTQueue tchan msg
    collectMsgs config h tchan
