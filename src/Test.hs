{-# LANGUAGE OverloadedStrings #-}

module Test where

import Network.HTTP
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as L
import Network (withSocketsDo)

test :: String -> Method -> Bool -> IO ()
test url meth sec = withSocketsDo $ do
            request' <- parseUrl url
            let request = request' { method = meth, secure = sec }
            manager <- newManager tlsManagerSettings
            res <- httpLbs request manager
            print (responseHeaders res)