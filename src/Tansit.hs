{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards, TypeOperators #-}

module Tansit (serverWorker, workersEndpoint) where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket_)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, (.=))
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Network.JsonRpc.Server
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.ZMQ4.Monadic
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Ae
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Tansit.Parser


type Server = ReaderT Request IO

data Request = Request {
             reqId :: B.ByteString, reqIdChain :: [B.ByteString],
             reqBody :: B.ByteString, tempDir :: FilePath,
             signAs :: String, debS3MVar :: MVar () }

data ShortListResult = ShortListResult { shortListPackageName :: Text, shortListVersion :: Text, shortListArch :: Text}
    deriving (Eq, Show)

type LongListResult = Map Text Text

instance ToJSON ShortListResult where
    toJSON (ShortListResult pn v arch) = Ae.object [
        "package_name" .= pn, "version" .= v, "arch" .= arch]

instance FromJSON B.ByteString where
    parseJSON = Ae.withText "Base64-encoded string" $ \t ->
        case B64.decode $ TE.encodeUtf8 t of
            Left err -> fail err
            Right b  -> return b


debS3FailedErrorNo, packageFileDoesntExistErrorNo, packageFileHashDoesntMatch, debS3OutputParseFailedErrorNo :: Int
debS3FailedErrorNo = 500
packageFileDoesntExistErrorNo = 501
packageFileHashDoesntMatch = 502
debS3OutputParseFailedErrorNo = -32000

workersEndpoint :: String
workersEndpoint = "inproc://workers"

serverWorker :: FilePath -> String -> MVar () -> ZMQ z ()
serverWorker tempDir signAs debS3MVar = do
    sock <- socket Router
    connect sock workersEndpoint

    forever $ do
        mayReq <- parseRequest <$> receiveMulti sock
        case mayReq of
            Just req -> handleRequest sock req
            Nothing  -> return ()

    where parseRequest :: [B.ByteString] -> Maybe Request
          parseRequest listParts = do
              parts <- NonEmpty.nonEmpty listParts
              let reqIdChain = NonEmpty.take 2 parts
              let reqId = reqIdChain !! 1
              let reqBody = NonEmpty.last parts
              return Request {..}

handleRequest :: Socket z Router -> Request -> ZMQ z ()
handleRequest sock req = do
    mayResp <- liftIO $ runReaderT (call serverMethods $ LB.fromStrict $ reqBody req) req
    case mayResp of
        Just body -> reply $ LB.toStrict body
        Nothing -> return ()
    where reply s = sendMulti sock $ NonEmpty.fromList $ reqIdChain req ++ [s]

serverMethods :: Methods Server
serverMethods = toMethods [shortList, longList, packageCopy, sendPackageData, uploadPackage]

commonArgs :: Text :+: Text :+: Text :+: Text :+: ()
commonArgs = Required "bucket" :+:
    Optional "codename" ("stable" :: Text) :+:
    Optional "component" ("main" :: Text) :+:
    Optional "arch" ("amd64" :: Text) :+: ()

commonWriteArgs :: Text :+: Bool :+: Text :+: Text :+: Text :+: Text :+: ()
commonWriteArgs = Optional "cache_control" ("" :: Text) :+: Optional "preserve_versions" False :+: commonArgs

shortList, longList, packageCopy, sendPackageData, uploadPackage :: Method Server

-- RPC Methods

sendPackageData = toMethod "send_package_data" f $ Required "file_name" :+: Required "data" :+: ()
    where f :: Text -> B.ByteString -> RpcResult Server Int
          f unsafeFileName bytes = do
            dir <- packageDir
            liftIO $ createDirectoryIfMissing True dir
            let fileName = sanitiseFileName unsafeFileName
            liftIO $ B.appendFile (dir </> Text.unpack fileName) bytes
            return 0
                
uploadPackage = toMethod "upload" f $ Required "file_name" :+: Required "file_sha256_hash" :+: commonWriteArgs
    where f :: Text -> Text -> Text -> Bool -> Text -> Text -> Text -> Text -> RpcResult Server Text
          f unsafeFileName fileHash cacheControl preserveVersions bucket codename component arch = do

            dir <- packageDir
            let fileName = sanitiseFileName unsafeFileName
            let pn = dir </> Text.unpack fileName
            pnExists <- liftIO $ doesFileExist pn
            unless pnExists $
                throwError $ rpcError packageFileDoesntExistErrorNo "file not uploaded yet"

            pToHash <- liftIO $ LB.readFile pn
            let pHash = byteStringToHex $ SHA256.hashlazy pToHash
            unless (Text.toLower pHash == Text.toLower fileHash) $
                throwError $ rpcError packageFileHashDoesntMatch "file doesn't match given hash"

            opts <- (:) <$> keyIdOpt <*> pure
              (writeArgsAsOpts cacheControl preserveVersions)
            out <- rpcRunDebS3WithCommonArgs "upload" opts [Text.pack pn] bucket codename component arch

            liftIO $ removeFile pn
            return out

shortList = toMethod "short_list" f commonArgs
    where f :: Text -> Text -> Text -> Text -> RpcResult Server [ShortListResult]
          f bucket codename component arch = do
            rawOut <- rpcRunDebS3WithCommonArgs "list" [] [] bucket codename component arch
            mapM lineToResult $ Text.lines rawOut
          lineToResult :: Text -> RpcResult Server ShortListResult
          lineToResult l = case Text.words l of
                             [n, v, a] -> return $ ShortListResult n v a
                             _ -> throwError $ rpcError debS3OutputParseFailedErrorNo $ Text.append "parse failed on line: " l

longList = toMethod "long_list" f commonArgs
    where f :: Text -> Text -> Text -> Text -> RpcResult Server [LongListResult]
          f bucket codename component arch = do
            rawOut <- rpcRunDebS3WithCommonArgs "list" [("long", Nothing)] [] bucket codename component arch
            case parseLongList rawOut of
                Left s -> throwError $ rpcError debS3OutputParseFailedErrorNo $ Text.pack s
                Right l -> return l

packageCopy = toMethod "copy" f $ Required "package" :+: Required "to_codename" :+: Required "to_component" :+: Optional "versions" [] :+: commonWriteArgs
    where f :: Text -> Text -> Text -> [Text] -> Text -> Bool -> Text -> Text -> Text -> Text -> RpcResult Server Text
          f package to_codename to_component versions cacheControl preserveVersions bucket codename component arch = 
              let baseOpts = writeArgsAsOpts cacheControl preserveVersions ++ case versions of
                    [] -> []
                    vs -> [("versions", Just $ Text.unwords vs)] in
                do
                    opts <- (:) <$> keyIdOpt <*> pure baseOpts
                    rpcRunDebS3WithCommonArgs "copy" opts [package, to_codename, to_component] bucket codename component arch
      
-- Running deb-s3

rpcRunDebS3WithCommonArgs :: Text -> [(Text, Maybe Text)] -> [Text] -> Text -> Text -> Text -> Text -> RpcResult Server Text
rpcRunDebS3WithCommonArgs cmd opts args bucket codename component arch =
    rpcRunDebS3 cmd ([("bucket", Just bucket), ("codename", Just codename),
                      ("component", Just component), ("arch", Just arch)] ++ opts) args

rpcRunDebS3 :: Text -> [(Text, Maybe Text)] -> [Text] -> RpcResult Server Text
rpcRunDebS3 cmd opts args = do
    mVar <- debS3MVar <$> ask
    (success, output, err) <- lockWhenWriting mVar $
        runDebS3 cmd opts args
    if success
        then return output
        else throwError $ rpcError debS3FailedErrorNo err
    where lockWhenWriting mv a = liftIO $
              if cmd `elem` ["list"]
                  then a
                  else bracket_ (takeMVar mv) (putMVar mv ()) a

runDebS3 :: Text -> [(Text, Maybe Text)] -> [Text] -> IO (Bool, Text, Text)
runDebS3 cmd opts posArgs = do
    infoLog $ "Running deb-s3 with args: " ++ show args
    (_, Just stdOut, Just stdErr, ph) <- createProcess debS3
    out <- TIO.hGetContents stdOut
    err <- TIO.hGetContents stdErr
    exitCode <- waitForProcess ph
    return (exitCode == ExitSuccess, out, err)
    where args = Text.unpack <$> cmd : posArgs ++ fmap makeOpt opts
          makeOpt (k, v) = Text.concat ["--", k, case v of
                                                   Just s -> Text.append "=" s
                                                   Nothing -> Text.empty]
          debS3 = (proc "deb-s3" args) {
              std_out = CreatePipe, std_err = CreatePipe }

-- Other helpers

packageDir :: RpcResult Server FilePath
packageDir = do
    Request {reqId, tempDir} <- ask
    return $ tempDir </> (Text.unpack . TE.decodeUtf8 . B64.encode) reqId

byteStringToHex :: B.ByteString -> Text
byteStringToHex = TE.decodeUtf8 . LB.toStrict . BB.toLazyByteString . BB.byteStringHex

sanitiseFileName :: Text -> Text
sanitiseFileName = Text.filter $ not . isPathSeparator

writeArgsAsOpts :: Text -> Bool -> [(Text, Maybe Text)]
writeArgsAsOpts cacheControl preserveVersions = 
    catMaybes [
        if Text.null cacheControl
           then Nothing
           else Just ("cache-control", Just cacheControl),
        if preserveVersions
           then Just ("preserve-versions", Nothing)
           else Nothing
        ]

keyIdOpt :: RpcResult Server (Text, Maybe Text)
keyIdOpt = do
    signAs <- signAs <$> ask
    return ("sign", Just $ Text.pack signAs)

-- Will probably do something more sophisticated in future
infoLog :: String -> IO ()
infoLog s = liftIO $ putStrLn s
