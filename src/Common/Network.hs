module Common.Network (parseUrl, addHeader) where

import           Data.Bifunctor     (first)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as ENC
import           Network.HTTP.Req   (Option, Scheme (Https), Url, header, useHttpsURI)
import           Polysemy           (Member, Sem)
import           Polysemy.Error     (Error, fromEither, throw)
import           Text.URI           (mkURI)

parseUrl :: Member (Error String) r
         => T.Text
         -> Sem r (Url 'Https, Option scheme)
parseUrl url = do
    uri <- fromEither . first show . mkURI $ url
    case useHttpsURI uri of
      Nothing     -> throw $ "Failed parsing HTTPS uri: " <> T.unpack url
      Just result -> return result

addHeader :: Option 'Https -> (T.Text, T.Text) -> Option 'Https
addHeader options (name, value) =
    options <> header (ENC.encodeUtf8 name) (ENC.encodeUtf8 value)
