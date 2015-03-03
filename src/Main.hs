{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Paths_tansit (version)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO.Temp
import System.ZMQ4.Monadic hiding (version)

import Tansit

data CmdFlag = Verbose | Version | BindEndpoint String | SignAs String
               deriving (Eq, Show)

options :: [OptDescr CmdFlag]
options =
       [ Option "v"  ["verbose"] (NoArg Verbose) "doesn't do anything at the moment"
       , Option "V?" ["version"] (NoArg Version) "show version number"
       , Option "b"  ["bind"]    (ReqArg BindEndpoint "ENDPOINT") "bind ENDPOINT"
       , Option "s"  ["sign-as"] (ReqArg SignAs "KEYID") "sign-as KEYID"
       ]

parseOpts :: [String] -> IO ([CmdFlag], [String])
parseOpts argv = 
    case getOpt Permute options argv of
      (o, n, [])   -> return (o,n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: tansit [OPTION...]"

main :: IO ()
main = do
    (opts, args) <- getArgs >>= parseOpts
    let (endpoint, signAs) = findOpts opts
    if Version `elem` opts
       then putStrLn $ showVersion version
       else startServer endpoint $! signAs
    where findOpts = foldl (\(i, j) o -> case o of
                                             BindEndpoint s -> (s, j)
                                             SignAs       s -> (i, s)
                                             _              -> (i, j))
                          ("ipc://tansit.ipc", error "--sign-as required")


startServer :: String -> String -> IO ()
startServer endpoint signAs =
    runZMQ $ withSystemTempDirectory "tansit-packages" $ \tempDir -> do
        liftIO $ putStrLn $ "Tansit starting!\nTemp dir: " ++ tempDir

        debS3MVar <- liftIO $ newMVar ()

        sock <- socket Router
        bind sock endpoint

        sockWorkers <- socket Dealer
        bind sockWorkers workersEndpoint

        replicateM_ 10 $ async $ serverWorker tempDir signAs debS3MVar

        liftIO $ putStrLn $ "Listening on " ++ endpoint
        proxy sock sockWorkers Nothing
