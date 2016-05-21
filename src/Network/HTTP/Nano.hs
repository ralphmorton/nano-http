{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Nano(
    module Network.HTTP.Nano.Types,
    module Network.HTTP.Nano.Instances,
    Network.HTTP.Conduit.Request,
    tlsManager,
    mkJSONData,
    http,
    http',
    httpJSON,
    buildReq,
    addHeaders
) where

import Network.HTTP.Nano.Types
import Network.HTTP.Nano.Instances

import Control.Exception (handle)
import Control.Lens (review, view)
import Control.Monad
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String (fromString)
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Conduit hiding (http)
import Network.HTTP.Types.Status (statusCode, statusMessage)

-- |Create an HTTP manager
tlsManager :: IO Manager
tlsManager = newManager tlsManagerSettings

-- |Create a JSON request body
mkJSONData :: ToJSON d => d -> RequestData
mkJSONData = JSONRequestData . JSONData

-- |Perform an HTTP request
http :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m) => Request -> m BL.ByteString
http req = view httpManager >>= safeHTTP . httpLbs req >>= return . responseBody

-- |Perform an HTTP request, ignoring the response
http' :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m) => Request -> m ()
http' req = http req >> return ()

-- |Perform an HTTP request, attempting to parse the response as JSON
httpJSON :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m, FromJSON b) => Request -> m b
httpJSON req = http req >>= asJSON

asJSON :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m, FromJSON b) => BL.ByteString -> m b
asJSON bs = case (decode bs) of
    Nothing -> throwError . review _ResponseParseError $ BL.unpack bs
    Just b -> return b

-- |Build a request
buildReq :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m) => HttpMethod -> URL -> RequestData -> m Request
buildReq mthd url dta = do
    breq <- safeHTTP $ parseUrl url
    return . attachRequestData dta $ breq { method = B.pack $ show mthd }

attachRequestData :: RequestData -> Request -> Request
attachRequestData NoRequestData req = req
attachRequestData (JSONRequestData (JSONData b)) req = req { requestBody = RequestBodyLBS (encode b) }
attachRequestData (UrlEncodedRequestData px) req = flip urlEncodedBody req $ fmap (bimap B.pack B.pack) px

-- |Add headers to a request
addHeaders :: [(String, String)] -> Request -> Request
addHeaders hx req = req { requestHeaders = ehx ++ nhx }
    where
    ehx = requestHeaders req
    nhx = fmap toHeader hx
    toHeader (n, v) = (fromString n, B.pack v)

safeHTTP :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m) => IO a -> m a
safeHTTP act = do
    res <- liftIO $ handle (return . Left) (act >>= return . Right)
    case res of
        Left ex -> throwError $ review _NetworkError ex
        Right r -> return r
