{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Configuration.Config (runConfigOnFile, runModuleConfig) where

import           Common.Exceptions
import           Configuration.Types
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Configurator      as C
import           Polysemy               (Embed, InterpreterFor, Members, Member, Sem)
import qualified Polysemy               as P
import qualified Polysemy.Error         as PE
import qualified Polysemy.Reader        as PR
import           TextShow               (TextShow, showt)

fetchConfig :: FilePath -> IO AppConfig
fetchConfig path = do
  cfg <- C.load [C.Required path]
  xandrUsername <- C.require cfg "xandr.username"
  xandrPassword <- C.require cfg "xandr.password"
  xandrBaseUrl  <- C.require cfg "xandr.baseUrl"
  xandrMemberID <- C.require cfg "xandr.memberId"
  expiryIn <- C.require cfg "xandr.expiryIn"
  let xandrExpiryIn = fromInteger expiryIn
  let xandr = XandrConfig{..}
  panelUsername <- C.lookup cfg "panel.username"
  panelPassword <- C.lookup cfg "panel.password"
  panelBaseUrl  <- C.require cfg "panel.baseUrl"
  let panel = PanelConfig{..}
  -- verbose       <- C.require cfg "logging.verbose"
  loggingLevel  <- C.require cfg "logging.loggingLevel"
  -- logFile       <- C.require cfg "logging.logfile"
  return $ AppConfig {..}

loadFromFile :: Members '[ Embed IO
                        , PR.Reader FilePath
                        , PE.Error String ] r
             => Sem r AppConfig
loadFromFile = do
  file <- PR.ask
  PE.fromExceptionVia showException $ fetchConfig file

runConfigOnFile :: Members [Embed IO, PE.Error String] r
                => FilePath
                -> InterpreterFor Config r
runConfigOnFile path =
  PR.runReader path . P.reinterpret (\case LoadConfig -> loadFromFile)

runModuleConfig :: Member Config r
                => InterpreterFor ModuleConfig r
runModuleConfig = P.interpret $ \case
  GetPanelConfig -> panel <$> loadConfig
  GetXandrConfig -> xandr <$> loadConfig
