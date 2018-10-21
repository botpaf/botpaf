{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bot.Config
  ( getBotConfig

  , BotConfig
  , ircConfig

  , IrcConfig
  , ircHost
  , ircPort
  , ircName
  , ircToken
  , ircRooms

  , (^.)

  ) where

import qualified Data.List

import Data.Bifunctor ( second )

import           Data.Text            ( Text )
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text ( readFile )

import Control.Exception ( IOException, try )

import Lens.Micro.Platform ( makeLenses, view, set, (^.), (.~) )

import Data.Ini.Config.Bidir

data BotConfig = BotConfig
  { _ircConfig :: IrcConfig
  } deriving ( Eq, Show )

data IrcConfig = IrcConfig
  { _ircHost  :: Text
  , _ircPort  :: Int
  , _ircName  :: Text
  , _ircToken :: Text
  , _ircRooms :: [Text]
  } deriving ( Eq, Show )

makeLenses ''BotConfig
makeLenses ''IrcConfig

configSpec :: IniSpec BotConfig ()
configSpec = do
  section "IRC" $ do
    ircConfig . ircHost  .= field "host" text
    ircConfig . ircPort  .= field "port" number
    ircConfig . ircName  .= field "name" text
    ircConfig . ircToken .= field "token" text
    ircConfig . ircRooms .= field "rooms" (listWithSeparator "," text)

defaultConfig = BotConfig
  { _ircConfig = IrcConfig
    { _ircHost  = "default"
    , _ircPort  = 0
    , _ircName  = "default"
    , _ircToken = "default"
    , _ircRooms = []
    }
  }

getBotConfig :: FilePath -> IO BotConfig
getBotConfig filename = do
  -- read ini config
  res <- parseIni <$> Text.readFile filename
                  <*> pure (ini defaultConfig configSpec)
  iniConfig <- case res of
    Left err     -> error err
    Right config -> pure $ getIniValue config
  -- read .env
  dotenv <- try @IOException $ Text.readFile ".env"
  assocs <- case dotenv of
    Left _   -> pure []
    Right xs -> pure . map (second Text.tail . Text.breakOn "=") . filter (not . Text.null) . Text.lines $ xs
  config <- pure $ maybe
    iniConfig
    (\dotenvToken -> set (ircConfig . ircToken) dotenvToken iniConfig)
    (snd <$> Data.List.find (("TWITCH_IRC_TOKEN" ==) . Text.toUpper . fst) assocs)
  pure $ config
