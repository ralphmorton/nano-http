{-# LANGUAGE TemplateHaskell #-}

module Network.HTTP.Nano.Types where

import Control.Lens.TH
import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Conduit (Manager)

type URL = String

data HttpMethod = OPTIONS | HEAD | GET | PUT | POST | DELETE | PATCH | CustomMethod String

showHttpMethod :: HttpMethod -> String
showHttpMethod OPTIONS = "OPTIONS"
showHttpMethod HEAD    = "HEAD"
showHttpMethod GET     = "GET"
showHttpMethod PUT     = "PUT"
showHttpMethod POST    = "POST"
showHttpMethod DELETE  = "DELETE"
showHttpMethod PATCH   = "PATCH"
showHttpMethod (CustomMethod m) = m

-- |HTTP config
newtype HttpCfg = HttpCfg {
    _httpManager :: Manager
}

-- |HTTP error
data HttpError =
    NetworkError HttpException |
    ResponseParseError String
    deriving Show

-- |Request Data
data RequestData
    = NoRequestData
    | JSONRequestData Value
    | UrlEncodedRequestData [(String, String)]
    | RawRequestData ByteString

makeClassy ''HttpCfg
makeClassyPrisms ''HttpError
