{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Xandr.API where

import           Common.Exceptions   (showException)
import           Common.Logging
import           Common.Network      (addHeader, parseUrl)
import           Configuration.Types (getXandrConfig, ModuleConfig, XandrConfig(..))
import           Data.Maybe          (listToMaybe)
import qualified Data.Text           as T
import           Network.HTTP.Req    (GET (..), HttpConfig, NoReqBody (..), Option, POST (..),
                                      ReqBodyJson (..), Scheme (Https), Url, defaultHttpConfig, jsonResponse,
                                      req, responseBody, runReq, (/:), (=:))
import           Polysemy            (Embed, Members, Sem)
import qualified Polysemy            as P
import qualified Polysemy.Error      as PE
import qualified Polysemy.Output     as PO
import qualified Polysemy.Reader     as PR
import           Xandr.LoginHandler  (LoginHandler (..), SessionToken (..), fetchToken)
import           Xandr.Types

type Code = T.Text

data XandrAPI m a where
  FetchCreativeInfo :: Code -> XandrAPI m (Maybe CreativeInfo)
  UploadBanner :: BannerCreative -> XandrAPI m ()
  UploadNative :: NativeCreative -> XandrAPI m ()

P.makeSem ''XandrAPI

runXandrApiOnReq :: Members '[ Embed IO
                            , LoginHandler
                            , ModuleConfig
                            , PO.Output LogMessage
                            , PE.Error String ] r
                 => Sem (XandrAPI ': r) a
                 -> Sem r a
runXandrApiOnReq sem = do
  (XandrConfig _ _ url memberId _) <- getXandrConfig
  (url', options) <- parseUrl url

  SessionToken token <- fetchToken
  let headers = options `addHeader` ("Authorization", token)

  P.interpret (\case
    FetchCreativeInfo code -> do
      let request' = req GET (url' /: "creative") NoReqBody jsonResponse (headers <> "member_id" =: memberId <> "code" =: code)
      res <- runReq defaultHttpConfig $ request'

      let response :: Maybe CreativeInfo = listToMaybe . responseCreatives $ responseBody res
      return response
    UploadBanner banner -> do
      let request' = req POST (url' /: "creative") (ReqBodyJson banner) jsonResponse (headers <> "member_id" =: memberId)
      res <- PE.fromExceptionVia showException $ runReq defaultHttpConfig $ request'

      let response :: UploadStatus = responseBody res

      logDebug $ T.pack ("Banner Upload Status " <> show response)

      if response == OKAY then return () else PE.throw $ "Failed parsing HTTPS uri: " <> show response
    UploadNative native -> do
      let request' = req POST (url' /: "creative") (ReqBodyJson native) jsonResponse (headers <> "member_id" =: memberId)
      res <- PE.fromExceptionVia showException $ runReq defaultHttpConfig $ request'

      let response :: UploadStatus = responseBody res

      logDebug $ T.pack ("Native Upload Status " <> show response)

      if response == OKAY then return () else PE.throw $ "Failed parsing HTTPS uri: " <> show response

    ) sem
