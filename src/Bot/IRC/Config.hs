{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot.IRC.Config
  ( Config(..)
  , parseConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Lens.Micro.TH (makeLenses)
import Data.Ini.Config.Bidir

data Config = Config
  { _host :: Text
  , _port :: Int
  , _name :: Text
  , _token :: Text
  } deriving (Eq, Show)

makeLenses ''Config

configSpec :: IniSpec Config ()
configSpec = do
  section "NETWORK" $ do
    host .= field "host" text
    port .= field "port" number
  section "AUTH" $ do
    name .= field "name" text
    token .= field "token" text

defaultConfig :: Config
defaultConfig = Config
  { _host = "default"
  , _port = 0
  , _name = "default"
  , _token = "default"
  }

-- TODO maybe don't throw exceptions this brutally
parseConfig :: FilePath -> IO Config
parseConfig filename = do
  Right config <- parseIni <$> Text.readFile filename
                           <*> pure (ini defaultConfig configSpec)
  pure $ getIniValue config
