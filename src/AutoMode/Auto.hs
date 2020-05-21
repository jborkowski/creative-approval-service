{-# LANGUAGE TemplateHaskell #-}
module AutoMode.Auto where

import qualified Polysemy as P

data Auto m a where
  Run :: Auto m ()

P.makeSem ''Auto
