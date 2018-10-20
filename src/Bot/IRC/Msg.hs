module Bot.IRC.Msg where

import Irc.RawIrcMsg ( TagEntry )
import Irc.Message   ( IrcMsg )

data Msg = Msg
  { _tags :: [TagEntry] -- TODO: this needs to be a strict ADT not a lazy list
  , _msg :: !IrcMsg
  } deriving (Show)
