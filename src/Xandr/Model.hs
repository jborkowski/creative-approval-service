{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Xandr.Model where

import Data.Aeson
import Data.Aeson.Casing

import Data.Text     as T
import GHC.Generics

data Template = NativeTemplate | BannerTemplate deriving (Show)

instance ToJSON Template where
  toJSON NativeTemplate = object [ "id" .= Number 39461 ]
  toJSON BannerTemplate = object [ "id" .= Number 4 ]

---------------------------------------------------------------------

newtype Url = Url T.Text deriving (Generic)

instance ToJSON Url where
  toJSON = genericToJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------
data Tracker = Tracker
  { trackerUrl       :: Url
  , trackerUrlSecure :: Url
  } deriving (Generic)

instance ToJSON Tracker where
   toJSON = genericToJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------
data Link = Link
  { linkUrl      :: Url
  , linkTrackers :: [Tracker]
  } deriving (Generic)

instance ToJSON Link where
   toJSON = genericToJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data Pixel = Pixel { pixelUrl       :: Maybe Url
                   , pixelSecureUrl :: Maybe Url
                   , pixelFormat    :: T.Text
                   } deriving (Generic)

instance ToJSON Pixel where
   toJSON = genericToJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data NativeAttr =
  NativeAttr { nativeAttrLink          :: Link
             , nativeAttrImageTrackers :: [Pixel]
             , nativeAttrDataAssets    :: [NativeAsset]
             , nativeAttrImageAssets   :: [ImageAsset]
             } deriving (Generic)

instance ToJSON NativeAttr where
   toJSON = genericToJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data Person = Person
    { personFirstName :: Text
    , personLastName  :: Text
    } deriving (Generic)

instance ToJSON Person where
    toJSON = genericToJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data NativeAsset =
  NativeAsset { nativeAssetDataType :: T.Text
              , nativeAssetValue    :: T.Text
              } deriving (Generic)

instance ToJSON NativeAsset where
    toJSON = genericToJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data ImageAsset =
  ImageAsset { imageAssetType               :: T.Text
             , imageAssetCreativeAssetImage :: Image
             } deriving (Generic)

instance ToJSON ImageAsset where
  toJSON = genericToJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data Image =
  Image { imageUrl       :: Url
        , imageUrlSecure :: Url
        , imageWidth     :: Int
        , imageHeight    :: Int
        } deriving (Generic)

instance ToJSON Image where
  toJSON = genericToJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data Creative =
   Banner { bannerClickTarget    :: T.Text
          , bannerCode           :: T.Text
          , bannerHeigh          :: Int
          , bannerWidth          :: Int
          , bannerMediaUrl       :: Url
          , bannerMediaUrlSecure :: Url
          , bannerPixels         :: [Pixel]
          , bannerTemplate       :: Template
          , bannerAllowAudit     :: Bool
          , bannerAllowSslAudit  :: Bool
          }
  | Native { nativeCode            :: T.Text
           , nativeMediaUrl        :: Url
           , nativeMediaUrlSecure  :: Url
           , nativeNativeAttribute :: NativeAttr
           , nativeTemplate        :: Template
           , nativeAllowAudit      :: Bool
           , nativeAllowSslAudit   :: Bool
           } deriving (Generic)

instance ToJSON Creative where
   toJSON = genericToJSON $ aesonPrefix snakeCase

bannerCreative :: T.Text -> T.Text -> Int -> Int -> Url -> Url -> [Pixel] -> Creative
bannerCreative clickTarget code heigh width mediaUrl mediaUrlSecure pixels =
  Banner { bannerClickTarget = clickTarget
         , bannerCode = code
         , bannerHeigh = heigh
         , bannerWidth = width
         , bannerMediaUrl = mediaUrl
         , bannerMediaUrlSecure = mediaUrlSecure
         , bannerPixels = pixels
         , bannerTemplate = BannerTemplate
         , bannerAllowAudit = True
         , bannerAllowSslAudit = True
         }

nativeCreative :: T.Text -> Url -> Url -> NativeAttr -> Creative
nativeCreative code mediaUrl mediaUrlSecure nativeAttr =
  Native { nativeCode = code
         , nativeMediaUrl = mediaUrl
         , nativeMediaUrlSecure = mediaUrlSecure
         , nativeNativeAttribute = nativeAttr
         , nativeTemplate = NativeTemplate
         , nativeAllowAudit = True
         , nativeAllowSslAudit = True
         }

---------------------------------------------------------------------

data AuditStatus = NoAudit | Pending | Rejected | Audited deriving (Show)

instance FromJSON AuditStatus where
  parseJSON = withText "AuditStatus" $ \case
    "no-audit" -> return NoAudit
    "pending"  -> return Pending
    "rejected" -> return Rejected
    "audited"  -> return Audited

---------------------------------------------------------------------

data SingleResponseRoot =
  SingleResponseRoot{ singleResponseRootRoot :: SingleResponse } deriving (Generic, Show)

instance FromJSON SingleResponseRoot where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data SingleResponse =
  SingleResponse { singleResponseStatus   :: T.Text
                 , singleResponseCreative :: Maybe ResponseCreative
                 } deriving (Generic, Show)

instance FromJSON SingleResponse where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data MultipleResponse =
  MultipleResponse { multipleResponseStatus       :: T.Text
                   , multipleResponseCount        :: Int
                   , multipleResponseStartElement :: Int
                   , multipleResponseNumElements  :: Int
                   , multipleResponseCreatives    :: [ResponseCreative]
                   } deriving (Generic, Show)

instance FromJSON MultipleResponse where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

---------------------------------------------------------------------

data ResponseCreative =
  ResponseCreative { responseCreativeId            :: Int
                   , responseCreativeActive        :: Bool
                   , responseCreativeIsExpired     :: Bool
                   , responseCreativeCode          :: T.Text
                   , responseCreativeAuditStatus   :: AuditStatus
                   , responseCreativeAuditFeedback :: Maybe T.Text
                   } deriving (Generic, Show)

instance FromJSON ResponseCreative where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
