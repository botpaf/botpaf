{-# LANGUAGE OverloadedStrings #-}

module Bot.IRC where

import Data.Foldable    ( for_ )
import Data.Traversable ( for )

import qualified Data.Text as Text
import Control.Exception

import Hookup

import Irc.Message   ( IrcMsg(..), cookIrcMsg )
import Irc.RawIrcMsg ( parseRawIrcMsg, asUtf8, RawIrcMsg, renderRawIrcMsg )

import Irc.Commands  ( ircPass, ircNick, ircPong )

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
  mb <- recvLine c 512
  for mb $ \xs ->
    case parseRawIrcMsg (asUtf8 xs) of
      Just msg -> return $! cookIrcMsg msg
      Nothing -> fail "Server sent invalid message"

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg h = send h . renderRawIrcMsg

sendHello :: Config -> Connection -> IO ()
sendHello config h = do
  sendMsg h (ircPass $ _token config)
  sendMsg h (ircNick $ _name config)

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
