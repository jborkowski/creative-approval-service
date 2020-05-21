{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Panel.Types where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Data
import qualified Data.Text         as T
import           Data.Typeable
import           GHC.Generics
import           TextShow
import           TextShow.TH


data Native = Native
  { nativeHash                         :: T.Text
  , nativeTargetUrl                    :: T.Text -- Should be Url ...
  , nativeWidth                        :: Int
  , nativeHeight                       :: Int
  , nativeDescriptionShort             :: Maybe T.Text
  , nativeDescriptionLong              :: Maybe T.Text
  , nativeTitleShort                   :: Maybe T.Text
  , nativeTitleLong                    :: Maybe T.Text
  , nativeTitleNormal                  :: Maybe T.Text
  , nativeExternalDisplayTrackingPixel :: Maybe T.Text -- url
  } deriving (Generic, Show)

instance FromJSON Native where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

---------------------------------------------------------------------

data Banner = Banner
  { bannerHash                         :: T.Text
  , bannerTargetUrl                    :: T.Text
  , bannerWidth                        :: Int
  , bannerHeight                       :: Int
  , bannerExternalDisplayTrackingPixel :: Maybe T.Text
  } deriving (Generic, Show)

instance FromJSON Banner where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

---------------------------------------------------------------------

data Region = EU | US

deriveTextShow ''Region

---------------------------------------------------------------------

data CreativeStatus = New | ReadyToSend | Pending | Accepted | Refused deriving (Typeable, Data)

instance TextShow CreativeStatus where
  showbPrec = undefined
  showt = T.toUpper . T.pack . snakeCase . showConstr . toConstr

---------------------------------------------------------------------

data Inventory = Natives | Banners deriving (Typeable, Data)

instance TextShow Inventory where
  showbPrec = undefined
  showt = T.toLower . T.pack . showConstr . toConstr
