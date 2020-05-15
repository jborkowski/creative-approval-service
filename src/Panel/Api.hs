{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Panel.Api where

import           Configuration           (PanelConfig (..))
import           Control.Monad.IO.Class
import           System.Log.FastLogger
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.Aeson                    as A
import qualified Data.ByteString         as BS
import           Data.Functor            ((<&>))
import qualified Data.Text               as T
import           Network.Connection      (TLSSettings (TLSSettingsSimple))
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Req
import           Panel.Model
import           Polysemy                (Embed, Members, Sem)
import qualified Polysemy                as P
import qualified Polysemy.Input          as PI
import qualified Polysemy.Reader         as PR
import           TextShow                (showt)
import           Data.Maybe                   (fromMaybe)



data PanelAPI m a where
  FetchBanners :: Region -> CreativeStatus -> PanelAPI m [Banner]

P.makeSem ''PanelAPI

a = fromMaybe

runPanelAPIonReq :: Members [ Embed IO, PR.Reader PanelConfig ] r
                 => Sem (PanelAPI : r) a
                 -> Sem r a
runPanelAPIonReq = P.reinterpret $ \case
  FetchBanners region status -> do
    config <- PR.ask
    conn  <- PI.input
    P.embed $ conn


username = "adpilotsp_bidder"
password = "2447417e965B@"
baseUrl = "https://api.adnxs.com"
memberId = 1523

url :: BS.ByteString
url = "https://panel.internal.cortb.pl/internal-api/creation-approve"


buildUrl :: BS.ByteString -> Region -> Inventory -> Maybe (Url 'Https)
buildUrl url region inventory = fst <$> parseUrlHttps url <&> \url -> url /: showt region /: showt inventory

fetchBanners' :: (MonadHttp m) => Region -> CreativeStatus -> m [Banner]
fetchBanners' region status = do
  case buildUrl url region Banners of
    Just url -> do
      responseBody <$> req GET url NoReqBody jsonResponse ("status" =: showt status)
    Nothing -> return []

test :: IO ()
test =
  runReq defaultHttpConfig $ do
    r <- fetchBanners EU New
    liftIO $ print (r)
