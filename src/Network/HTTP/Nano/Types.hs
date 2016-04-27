{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.HTTP.Nano.Types where

import Control.Lens.TH
import Data.Aeson (ToJSON)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Conduit (Manager)

type URL = String

data HttpMethod = OPTIONS | HEAD | GET | PUT | POST | DELETE | CustomMethod String

-- |HTTP config
data HttpCfg = HttpCfg {
    _httpManager :: Manager
}

-- |HTTP error
data HttpError =
    NetworkError HttpException |
    ResponseParseError String
    deriving Show

-- |Existential container for JSON data
data JSONData = forall d . ToJSON d => JSONData d

-- |Request Data
data RequestData = NoRequestData | JSONRequestData JSONData | UrlEncodedRequestData [(String, String)]

makeClassy ''HttpCfg
makeClassyPrisms ''HttpError
