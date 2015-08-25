{-# LANGUAGE OverloadedStrings #-}

module Init where


import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Char8 (pack)
import Data.Char

import Test
import Public


initial :: IO ()
initial = do
    putStrLn "What do you want to run? [test] / [public] / [trade]"
    input <- getLine
    case input of
        "test" -> do
            putStrLn "What url? [www.example.com]"
            url' <- getLine
            if (take 4 url') /= "www."
                then do
                    putStrLn "\nPlease enter a valid URL. (starting with \"www.\")\n~~ (Aborted) ~~\n"
                    initial
                else do
                    putStrLn "https? [y/n]"
                    sec' <- getLine
                    if sec' == "y" || sec' == "n"
                        then do
                            putStrLn "What method? [GET / HEAD]"
                            meth' <- getLine
                            let meth'' = map toLower meth'
                            if meth'' == "get" || meth'' == "head"
                                then do
                                    let meth = pack (map toUpper meth')
                                    case sec' of
                                        "y" -> do
                                            let url = "https://" ++ url'
                                            let sec = True
                                            test url meth sec
                                        "n" -> do
                                            let url = "http://" ++ url'
                                            let sec = False
                                            test url meth sec
                                else do
                                    putStrLn "\nPlease just type \"GET\" or \"HEAD\"\n~~ (Aborted) ~~\n"
                                    initial
                        else do
                            putStrLn "\nPlease just type \"y\" or \"n\"\n~~ (Aborted) ~~\n"
                            initial
        "public" -> putStrLn "\"public\" - not yet implemented"
        "trade" -> putStrLn "\"trade\" - not yet implemented"
        _ -> putStrLn "  X__X"