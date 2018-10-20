{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Bot.IRC where

import Data.Foldable    ( for_, foldl' )
import Data.Traversable ( for )

import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text ( putStrLn )
import qualified Data.Text.Encoding as Text ( decodeUtf8 )
import Control.Exception

import Hookup

import Irc.Message   ( IrcMsg(..), cookIrcMsg )
import Irc.RawIrcMsg ( parseRawIrcMsg, asUtf8, RawIrcMsg, renderRawIrcMsg, msgTags )

import Irc.Commands  ( ircCapReq, ircPass, ircNick, ircPong, ircJoin )

import Lens.Micro.Platform ( (^.) )

import Bot.IRC.Config
import Bot.IRC.Msg

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
readIrcLine :: Connection -> IO (Maybe Msg)
readIrcLine c = do
  mb <- recvLine c 1024 -- RFC 1459 «512 for tags + 512 for standard msg»
  for mb $ \xs -> do
    Text.putStrLn $ Text.decodeUtf8 xs
    case parseRawIrcMsg (asUtf8 xs) of
      Just msg -> return $! Msg { _tags = msg ^. msgTags
                                , _msg  = cookIrcMsg msg }
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

-- groups the channels to join in batches and joins them via a single command
--   JOIN #foo,#bar,#baz,#quux
sendJoin :: [Text] -> Connection -> IO ()
sendJoin channels h = do
  let css = group (510 - Text.length "JOIN :") channels
  for_ css $ \cs -> do
    sendMsg h (ircJoin (Text.intercalate "," cs) Nothing)

  where

    group max channels =
      case foldl' (collect max) (0,[],[]) channels of
        (l, xs, xss) | l > 0     -> xs:xss
                     | otherwise -> xss

    collect max (l,xs,xss) x@(Text.length -> lx)
      | l+lx <= max = ( l + lx, x : xs,          xss )
      | otherwise   = (      0,     [], (x:xs) : xss )

-- test event loop, to be transitioned to action to produce TQueue
eventLoop :: Config -> Connection -> IO ()
eventLoop config h = do
  mb <- readIrcLine h
  for_ mb $ \msg -> do
    print msg
    case msg of
      Msg _ (Ping xs) -> sendMsg h (ircPong xs)
      _               -> pure ()
    eventLoop config h
