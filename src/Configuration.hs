{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Configuration (XandrConfig(..), PanelConfig(..), Config(..)) where

import qualified Data.Text as T
import qualified Data.Configurator as C

data XandrConfig = XandrConfig
  { username :: T.Text
  , password :: T.Text
  , baseUrl  :: T.Text
  , memberID :: Int
  } deriving (Show, Eq)

data PanelConfig = PanelConfig
  { username :: Maybe T.Text
  , password :: Maybe T.Text
  , baseUrl  :: T.Text
  } deriving (Show, Eq)

data Config = Config
  { loggingLeverl :: Int
  , xandr :: XandrConfig
  , panel :: PanelConfig
  } deriving (Show, Eq)
