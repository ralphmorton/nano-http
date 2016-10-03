{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Nano(
    module Network.HTTP.Nano.Types,
    Network.HTTP.Conduit.Request,
    tlsManager,
    mkJSONData,
    http,
    http',
    httpS,
    httpSJSON,
    httpJSON,
    buildReq,
    addHeaders
) where

import Network.HTTP.Nano.Types

import Control.Exception (handle)
import Control.Lens (review, view)
import Control.Monad
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson (FromJSON, ToJSON (..), eitherDecode, encode)
import Data.Bifunctor (bimap)
import Data.Conduit (ResumableSource, Conduit, ($=+))
import Data.JsonStream.Parser (value, parseByteString)
import Data.String (fromString)
import Network.HTTP.Conduit hiding (http)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Conduit.List as CL
import qualified Network.HTTP.Conduit as HC

-- |Create an HTTP manager
tlsManager :: IO Manager
tlsManager = newManager tlsManagerSettings

-- |Create a JSON request body
mkJSONData :: ToJSON d => d -> RequestData
mkJSONData = JSONRequestData . toJSON

-- |Perform an HTTP request
http :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m) => Request -> m BL.ByteString
http req = responseBody <$> (view httpManager >>= safeHTTP . httpLbs req)

-- |Perform an HTTP request, ignoring the response
http' :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m) => Request -> m ()
http' = void . http

httpS :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadResource m) => Request -> m (ResumableSource m B.ByteString)
httpS req = responseBody <$> (view httpManager >>= HC.http req)

httpSJSON :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadResource m, FromJSON a) => Request -> m (ResumableSource m a)
httpSJSON req = do
    src <- httpS req
    return $ src $=+ jsonConduit

jsonConduit :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadResource m, FromJSON a) => Conduit B.ByteString m a
jsonConduit = CL.mapFoldable (parseByteString value)

-- |Perform an HTTP request, attempting to parse the response as JSON
httpJSON :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m, FromJSON b) => Request -> m b
httpJSON req = http req >>= asJSON

asJSON :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m, FromJSON b) => BL.ByteString -> m b
asJSON bs = case eitherDecode bs of
    Left err -> throwError (review _ResponseParseError (err ++ "; original data: " ++ BL.unpack bs))
    Right b -> return b

-- |Build a request
buildReq :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m) => HttpMethod -> URL -> RequestData -> m Request
buildReq mthd url dta = do
    breq <- safeHTTP $ parseUrlThrow url
    return . attachRequestData dta $ breq { method = B.pack $ showHttpMethod mthd }

attachRequestData :: RequestData -> Request -> Request
attachRequestData NoRequestData req = req
attachRequestData (JSONRequestData b) req = req { requestBody = RequestBodyLBS (encode b) }
attachRequestData (UrlEncodedRequestData px) req = flip urlEncodedBody req $ fmap (bimap B.pack B.pack) px
attachRequestData (RawRequestData bs) req = req { requestBody = RequestBodyLBS bs }

-- |Add headers to a request
addHeaders :: [(String, String)] -> Request -> Request
addHeaders hx req = req { requestHeaders = ehx ++ nhx }
    where
    ehx = requestHeaders req
    nhx = fmap toHeader hx
    toHeader (n, v) = (fromString n, B.pack v)

safeHTTP :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, MonadIO m) => IO a -> m a
safeHTTP act = do
    res <- liftIO $ handle (return . Left) (Right <$> act)
    case res of
        Left ex -> throwError $ review _NetworkError ex
        Right r -> return r
