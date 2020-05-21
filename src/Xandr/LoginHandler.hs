{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Xandr.LoginHandler (runLoginHandler, LoginHandler(..), fetchToken, SessionToken(..)) where

import           Common.Logging
import           Common.Network
import           Configuration.Types    (getXandrConfig, XandrConfig (..), ModuleConfig)
import           Control.Monad          (guard)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson             as A
import           Data.Aeson.Casing
import qualified Data.Text              as T
import           Data.Time              (UTCTime, diffUTCTime, getCurrentTime)
import           GHC.Generics           (Generic)
import           Network.HTTP.Req
import           Polysemy               (Embed, InterpreterFor, Member, Members, Sem)
import qualified Polysemy               as P
import qualified Polysemy.Error         as PE
import qualified Polysemy.Output        as PO
import qualified Polysemy.Reader        as PR
import qualified Polysemy.State         as PS


newtype SessionToken = SessionToken T.Text
    deriving (Show)

data LoginHandler m a where
  FetchToken :: LoginHandler m SessionToken

data Credentials = Credentials
  { credentialsUsername :: T.Text
  , credentialsPassword :: T.Text
  } deriving (Generic, Show)

instance A.ToJSON Credentials where
  toJSON = A.genericToJSON $ aesonPrefix snakeCase

data AuthorizationRequest = AuthorizationRequest
  { authorizationAuth :: Credentials } deriving (Generic, Show)

instance A.ToJSON AuthorizationRequest where
  toJSON = A.genericToJSON $ aesonPrefix snakeCase

data LoginResponse = LoginResponse
  { loginResponseToken   :: T.Text
  , loginResponseStatus  :: T.Text
  } deriving (Show, Generic)

instance A.FromJSON LoginResponse where
  parseJSON = A.withObject "response" $ \o -> do
    r <- o A..: "response"
    loginResponseToken  <- r A..: "token"
    loginResponseStatus <- r A..: "status"
    return LoginResponse{..}

data SessionTokenWithTime = SessionTokenWithTime
  { _stwtToken            :: SessionToken
  , _stwtTokenRetrievedAt :: UTCTime
  } deriving (Show)

type LoginState = Maybe SessionTokenWithTime

P.makeSem ''LoginHandler

-- fetchToken :: Member LoginHandler r => Sem r SessionToken

fetchSessionToken :: Members '[ Embed IO
                              , PO.Output LogMessage
                              , ModuleConfig
                              , PE.Error String] r
                  => Sem r SessionToken
fetchSessionToken = do
  (XandrConfig user pass bUrl _ _) <- getXandrConfig
  (url, option) <- parseUrl bUrl
  let request = createLoginRequest url option user pass

  response <- runReq defaultHttpConfig request
  logDebug $ either (T.pack . ("Failed to fetch token: " <>))
                      (const "Successfully fetched token") response

  PE.fromEither response

createLoginRequest :: (A.ToJSON AuthorizationRequest) => Url 'Https
                   -> Option 'Https
                   -> T.Text
                   -> T.Text
                   -> Req (Either String SessionToken)
createLoginRequest url defaultOptions user pass = do
  let json = AuthorizationRequest ( Credentials user pass )
  response <- req POST (url /: "auth") (ReqBodyJson json) jsonResponse defaultOptions

  let loginResponse = responseBody response

  let result = if loginResponseStatus loginResponse == "OK" then
                    Right $ SessionToken $ loginResponseToken loginResponse
                 else Left $ show loginResponse
  return result

fetchTokenThroughCache :: Members '[ Embed IO
                                   , PO.Output LogMessage
                                   , PS.State LoginState
                                   , ModuleConfig
                                   , PE.Error String] r
                       => Sem r SessionToken
fetchTokenThroughCache = do
  currentTime <- liftIO getCurrentTime
  state <- PS.get
  (XandrConfig _ _ _ _ expiry) <- getXandrConfig

  case getCachedToken currentTime state expiry of
    Just token -> return token
    Nothing -> do
      token <- fetchSessionToken
      PS.put $ Just $ SessionTokenWithTime token currentTime
      return token
  where
    getCachedToken currentTime state expiry = do
      SessionTokenWithTime token retrievedAt <- state
      guard $ diffUTCTime currentTime retrievedAt < expiry
      return token


runLoginHandler :: Members [ Embed IO
                           , ModuleConfig
                           , PO.Output LogMessage
                           , PE.Error String ] r
                => InterpreterFor LoginHandler r
runLoginHandler sem = do
  fmap snd
  . PS.runState (Nothing :: LoginState)
  $ P.reinterpret (\case FetchToken -> fetchTokenThroughCache) sem
