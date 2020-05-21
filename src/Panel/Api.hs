{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Panel.API where

import           Common.Exceptions       (showException)
import           Common.Logging
import           Common.Network          (parseUrl)
import           Configuration.Types     (getPanelConfig, ModuleConfig, PanelConfig (..))
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Data.Aeson              as A
import           Data.Functor            ((<&>))
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T
import           Network.Connection      (TLSSettings (TLSSettingsSimple))
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Req
import           Panel.Types
import           Polysemy                (Embed, Members, Sem)
import qualified Polysemy                as P
import           Polysemy.Error
import qualified Polysemy.Input          as PI
import qualified Polysemy.Output         as PO
import qualified Polysemy.Reader         as PR
import           TextShow                (TextShow, showt)



data PanelAPI m a where
  FetchBanners :: Region -> CreativeStatus -> PanelAPI m [Banner]
  FetchNatives :: Region -> CreativeStatus -> PanelAPI m [Native]
  -- SetState :: UUID -> CreativeStatus -> PanelAPI m ()

P.makeSem ''PanelAPI

runPanelApiOnReq :: Members '[ Embed IO
                             , ModuleConfig
                             , PO.Output LogMessage
                             , Error String] r
                 => Sem (PanelAPI ': r) a
                 -> Sem r a
runPanelApiOnReq sem = do
  (PanelConfig _ _ url) <- getPanelConfig
  (url', options) <- parseUrl url
  P.interpret (\case
    FetchBanners region status -> do
      response <- fromExceptionVia showException $ runReq defaultHttpConfig $ buildRequest url' region Banners status
      let banners :: [Banner] = responseBody response
      return banners
    FetchNatives region status -> do
      response <- fromExceptionVia showException $ runReq defaultHttpConfig $ buildRequest url' region Natives status
      let natives :: [Native] = responseBody response
      return natives
    ) sem
    where
      buildRequest :: (MonadHttp m, A.FromJSON b) => Url scheme -> Region -> Inventory -> CreativeStatus -> m (JsonResponse b)
      buildRequest url region inventory status = req GET (url /: showt region /: showt inventory) NoReqBody jsonResponse ("status" =: showt status)
