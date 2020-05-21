module Common.Exceptions (showException) where

import qualified Control.Exception as CE

showException :: CE.SomeException -> String
showException = show
