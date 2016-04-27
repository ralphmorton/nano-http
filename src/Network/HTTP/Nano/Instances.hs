
module Network.HTTP.Nano.Instances() where

import Network.HTTP.Nano.Types

instance Show HttpMethod where
    show OPTIONS = "OPTIONS"
    show HEAD = "HEAD"
    show GET = "GET"
    show PUT = "PUT"
    show POST = "POST"
    show DELETE = "DELETE"
    show (CustomMethod m) = m
