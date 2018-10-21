{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Twitch.IRCv32 where

import Data.Text ( Text )
-- import qualified Data.Text as Text

import Lens.Micro.Platform ( makeLenses, view )

import Control.Concurrent.Async ( withAsync )
import Control.Concurrent.STM ( atomically, newTQueueIO, TQueue, readTQueue, writeTQueue )

import           Twitch.IRC        ( (.!) )
import qualified Twitch.IRC as IRC

import Bot.Config

data Room
  = Room
    { _roomId      :: Text     -- room-id
    , _roomName    :: Text     --
    }
  deriving (Eq, Show)

data User
  = User
    { _userId      :: Text     -- user-id
    , _userName    :: Text     -- display-name
    , _userMod     :: Bool     -- mod
    , _userSub     :: Bool     -- subscriber
    , _userBadges  :: Text     -- badges (TODO: [Badge])
    }
  deriving (Eq, Show)

data Message
  = Privmsg
    { _msgTs       :: Text     -- tmi-sent-ts (server received timestamp)
    , _msgRoom     :: Room
    , _msgUser     :: User
    , _msgText     :: Text
    }
  | IrcCore
    { _coreRaw     :: IRC.RawIrcMsg
    }
  deriving (Eq, Show)

data Badge
  = Admin | Bits | Broadcaster | GlobalMod | Moderator | Subscriber | Staff | Turbo
  deriving (Eq, Show)

makeLenses ''Room
makeLenses ''User
makeLenses ''Message

process :: IRC.RawIrcMsg -> Message
process m@(view IRC.msgCommand -> "PRIVMSG")
  = let [room,text] = view IRC.msgParams m in
    Privmsg { _msgTs   = m .! "tmi-sent-ts"
            , _msgRoom = Room { _roomId = m .! "room-id"
                              , _roomName = room
                              }
            , _msgUser = User { _userId     =        m .! "user-id"
                              , _userName   =        m .! "display-name"
                              , _userMod    = "1" == m .! "mod"
                              , _userSub    = "1" == m .! "subscriber"
                              , _userBadges =        m .! "badges"
                              }
            , _msgText = text
            }
process m = IrcCore m

connect :: BotConfig -> TQueue Message -> IO ()
connect config tchan = do
  irc <- newTQueueIO
  withAsync (IRC.connect config irc) $ const $ relay irc

  where
    relay irc = do
      msg <- atomically . readTQueue $ irc
      atomically $ writeTQueue tchan $! process msg
      relay irc
