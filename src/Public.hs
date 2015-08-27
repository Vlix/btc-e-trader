{-# LANGUAGE OverloadedStrings #-}

module Public where


import Network.HTTP hiding (Response)
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as L
import Network (withSocketsDo)

getBTCEHeaderResponse :: IO (Response L.ByteString)
getBTCEHeaderResponse = withSocketsDo $ do
                            req' <- parseUrl "https://btc-e.com/"
                            let req = req' { method = "HEAD", secure = True }
                            manager <- newManager tlsManagerSettings
                            result <- httpLbs req manager
                            return result

getBTCECookie :: IO (Response L.ByteString) -> IO Cookie
getBTCECookie result = do
                        cookiejar' <- result
                        let cookie = head (destroyCookieJar (responseCookieJar cookiejar'))
                        return cookie

--ctHeader :: Network.HTTP.Header
--ctHeader = ("Content-type","application/x-www-form-urlencoded")


public :: IO ()
public = withSocketsDo $ do
            req' <- parseUrl "https://btc-e.com/api/3/ticker/btc_usd"
            let req = req' {secure = True}
            manager <- newManager tlsManagerSettings
            result <- httpLbs req manager
            print $ responseBody result
            putStrLn "--------------------------------"
            cookie <- getBTCECookie getBTCEHeaderResponse
            print cookie