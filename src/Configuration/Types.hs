{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Configuration.Types  where

import qualified Data.Text       as T
import           Data.Time.Clock (NominalDiffTime)
import qualified Polysemy        as P

data XandrConfig = XandrConfig
  { xandrUsername :: T.Text
  , xandrPassword :: T.Text
  , xandrBaseUrl  :: T.Text
  , xandrMemberID :: Int
  , xandrExpiryIn :: NominalDiffTime
  } deriving (Show, Eq)

data PanelConfig = PanelConfig
  { panelUsername :: Maybe T.Text
  , panelPassword :: Maybe T.Text
  , panelBaseUrl  :: T.Text
  } deriving (Show, Eq)

data AppConfig = AppConfig
  { loggingLevel :: Int
  , xandr :: XandrConfig
  , panel :: PanelConfig
  } deriving (Show, Eq)

data Config m a where
  LoadConfig :: Config m AppConfig

P.makeSem ''Config

data ModuleConfig m a where
  GetXandrConfig :: ModuleConfig m XandrConfig
  GetPanelConfig :: ModuleConfig m PanelConfig

P.makeSem ''ModuleConfig
