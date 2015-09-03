{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Public where

import Prelude hiding (last)

import GHC.Generics
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Data.Maybe
import Control.Monad

import Data.Time

import Network.HTTP hiding (Response)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network (withSocketsDo)

public :: IO ()
public = withSocketsDo $ do
            req' <- parseUrl "https://btc-e.com/api/3/ticker/btc_usd"
            let req = req' {secure = True}
            manager <- newManager tlsManagerSettings
            result <- httpLbs req manager
            let btcUSDticker = unMaybeCur (decode $ responseBody result)
            let localTime' = fromJust $ parseTime defaultTimeLocale "%s" $ show (updated btcUSDticker) :: UTCTime
            localTimeZone <- getCurrentTimeZone
            let localTime = utcToLocalTime localTimeZone localTime'

            print $ responseBody result
            putStrLn "\n--------------------------------\n"
            print localTime
            print $ "Buy:  " ++ show (buy btcUSDticker :: Float)
            print $ "Sell: " ++ show (sell btcUSDticker :: Float)
            print $ "Avg:  " ++ show (avg btcUSDticker :: Float)
            putStrLn "\n--------------------------------\n"
            cookie <- getBTCECookie getBTCEHeaderResponse
            print cookie

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
                        let cookie = head . destroyCookieJar . responseCookieJar $ cookiejar'
                        return cookie

--ctHeader :: Network.HTTP.Header
--ctHeader = ("Content-type","application/x-www-form-urlencoded")

data Currency = Currency { btc_usd :: Ticker } deriving (Show, Generic)

data Ticker = Ticker { high :: Float
                     , low :: Float
                     , avg :: Float
                     , vol :: Float
                     , vol_cur :: Float
                     , last :: Float
                     , buy :: Float
                     , sell :: Float
                     , updated :: Integer
                     }  deriving (Show, Generic)

instance FromJSON Currency
instance FromJSON Ticker 

emptyTicker :: Ticker
emptyTicker = Ticker { high = 1.0
                     , low = 1.0
                     , avg = 1.0
                     , vol = 1.0
                     , vol_cur = 1.0
                     , last = 1.0
                     , buy = 1.0
                     , sell = 1.0
                     , updated = 1
                     }


unMaybeCur :: Maybe Currency -> Ticker
unMaybeCur Nothing = emptyTicker
unMaybeCur (Just Currency {btc_usd = x}) = x

