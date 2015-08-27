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
            print res
            putStrLn "`----------------------------"
            print request
            putStrLn "`----------------------------"


-- cookie id = d289ac5ff3e5d059814c92b0acf4dd7da1440704160