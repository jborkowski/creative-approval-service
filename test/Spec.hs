{-# LANGUAGE OverloadedStrings #-}
import Common.Logging
import Configuration.Config   (runConfigOnFile, runModuleConfig)
import Configuration.Types    (PanelConfig (..), XandrConfig (..), AppConfig (..), Config(..), loadConfig)
import Control.Monad.IO.Class (liftIO)
import Panel.API              (PanelAPI (..), fetchBanners, runPanelApiOnReq)
import Panel.Types            (CreativeStatus (..), Region (..))
import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Output
import Polysemy.Reader
import Polysemy.Resource
import Polysemy.State
import Configuration.Config
import Xandr.API              (XandrAPI, fetchCreativeInfo, runXandrApiOnReq)
import Xandr.LoginHandler     (runLoginHandler)

panelTest :: Members [Embed IO, PanelAPI, XandrAPI, Config ] r => Sem r ()
panelTest = do
  banners <- fetchBanners EU Accepted
  let size = show . length $ banners
  info <- fetchCreativeInfo "0jeG6nRaTUNrEJDvpnh8"
  cfg <- loadConfig
   -- liftIO $ putStrLn (show cfg)
  liftIO $ putStrLn (show info)

main :: IO ()
main = do
  res <- runFinal
    . embedToFinal
    . errorToIOFinal
    . outputToIOMonoid (:[])
    . outputToIOMonoid putStrLn
    . runConfigOnFile "config/application.config"
    . runModuleConfig
     -- . ignoreOutput
    . runPanelApiOnReq
    . runLoginHandler
    . runXandrApiOnReq
    $ panelTest
  either print (const $ pure ()) res
