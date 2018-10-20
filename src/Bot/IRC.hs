{-# LANGUAGE OverloadedStrings #-}

module Bot.IRC where

import Data.Foldable    ( for_ )
import Data.Traversable ( for )

import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text ( putStrLn )
import qualified Data.Text.Encoding as Text ( decodeUtf8 )
import Control.Exception

import Hookup

import Irc.Message   ( IrcMsg(..), cookIrcMsg )
import Irc.RawIrcMsg ( parseRawIrcMsg, asUtf8, RawIrcMsg, renderRawIrcMsg )

import Irc.Commands  ( ircCapReq, ircPass, ircNick, ircPong, ircJoin )

import Bot.IRC.Config

withConnection :: Config -> (Connection -> IO a) -> IO a
withConnection config = bracket (connect $ mkParams config) close
  where
    mkParams config = ConnectionParams
      { cpHost = Text.unpack $ _host config
      , cpPort = fromIntegral $ _port config
      , cpTls = Just TlsParams { tpClientCertificate = Nothing
                               , tpClientPrivateKey  = Nothing
                               , tpServerCertificate = Nothing
                               , tpCipherSuite       = "HIGH"
                               , tpInsecure          = False }
      , cpSocks = Nothing
      , cpFamily = defaultFamily
      }

-- TODO handle exceptions?
-- https://hackage.haskell.org/package/hookup-0.2.2/docs/Hookup.html#v:recvLine
readIrcLine :: Connection -> IO (Maybe IrcMsg)
readIrcLine c = do
  mb <- recvLine c 1024 -- RFC 1459 «512 for tags + 512 for standard msg»
  for mb $ \xs -> do
    Text.putStrLn $ Text.decodeUtf8 xs
    case parseRawIrcMsg (asUtf8 xs) of
      Just msg -> return $! cookIrcMsg msg
      Nothing -> fail "Server sent invalid message"

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg h = send h . renderRawIrcMsg

sendHello :: Config -> Connection -> IO ()
sendHello config h = do
  sendMsg h (ircCapReq ["twitch.tv/tags"])
  sendMsg h (ircCapReq ["twitch.tv/membership"])
  sendMsg h (ircCapReq ["twitch.tv/commands"])
  sendMsg h (ircPass $ _token config)
  sendMsg h (ircNick $ _name config)

sendJoin :: Text -> Connection -> IO ()
sendJoin channel h = do
  sendMsg h (ircJoin channel Nothing)

-- test event loop, to be transitioned to action to produce TQueue
eventLoop :: Config -> Connection -> IO ()
eventLoop config h = do
  mb <- readIrcLine h
  for_ mb $ \msg -> do
    print msg
    case msg of
      Ping xs -> sendMsg h (ircPong xs)
      _       -> pure ()
    eventLoop config h
