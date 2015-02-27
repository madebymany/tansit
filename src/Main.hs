{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards, TypeOperators #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, (.=))
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Network.JsonRpc.Server
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Temp
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

import Tansit.Parser

type Server = ReaderT Request IO

data Request = Request { reqId :: B.ByteString, reqBody :: B.ByteString, tempDir :: FilePath }
    deriving (Eq, Show)

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

main :: IO ()
main = do
    endpoint <- getEndpoint <$> getArgs
    runZMQ $ withSystemTempDirectory "tansit-packages" $ \tempDir -> do
        liftIO $ putStrLn $ "temp dir: " ++ tempDir
        sock <- socket Router
        bind sock endpoint
        forever $ handleClient sock tempDir
    where getEndpoint as = case as of
                             []    -> "ipc://tansit.ipc"
                             e : _ -> e

handleClient :: Socket z Router -> FilePath -> ZMQ z ()
handleClient sock tempDir = do
    mayReq <- parseRequest <$> receiveMulti sock
    case mayReq of
        Just req -> handleRequest sock req
        Nothing -> return () 

    where parseRequest :: [B.ByteString] -> Maybe Request
          parseRequest parts = do
              reqId <- safeHead parts
              let reqBody = fromMaybe B.empty $ safeLast parts
              return Request {..}
              where safeHead [] = Nothing
                    safeHead (x:_) = Just x
                    safeLast [] = Nothing
                    safeLast x = Just $ last x

handleRequest :: Socket z Router -> Request -> ZMQ z ()
handleRequest sock req = do
    mayResp <- liftIO $ runReaderT (call serverMethods $ LB.fromStrict $ reqBody req) req
    case mayResp of
        Just body -> reply $ LB.toStrict body
        Nothing -> return ()
    where reply s = sendMulti sock $ NonEmpty.fromList [reqId req, s]

serverMethods :: Methods Server
serverMethods = toMethods [shortList, longList, packageCopy, sendPackageData, uploadPackage]

commonArgs :: a :+: (Text :+: (Text :+: (Text :+: ())))
commonArgs = Required "bucket" :+:
             Optional "codename" ("stable" :: Text) :+:
             Optional "component" ("main" :: Text) :+:
             Optional "arch" ("amd64" :: Text) :+: ()

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
                
uploadPackage = toMethod "upload" f (Required "file_name" :+: Required "file_sha256_hash" :+: commonArgs)
    where f :: Text -> Text -> Text -> Text -> Text -> Text -> RpcResult Server Text
          f unsafeFileName fileHash bucket codename component arch = do
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
            out <- rpcRunDebS3WithCommonArgs "upload" [] [Text.pack pn] bucket codename component arch
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

packageCopy = toMethod "copy" f $ Required "package" :+: Required "to_codename" :+: Required "to_component" :+: Optional "versions" [] :+: commonArgs
    where f :: Text -> Text -> Text -> [Text] -> Text -> Text -> Text -> Text -> RpcResult Server Text
          f package to_codename to_component versions bucket codename component arch =
            let opts = case versions of
                         [] -> []
                         vs -> [("versions", Just $ Text.unwords vs)] in
                rpcRunDebS3WithCommonArgs "copy" opts [package, to_codename, to_component] bucket codename component arch
          
-- Running deb-s3

rpcRunDebS3WithCommonArgs :: Text -> [(Text, Maybe Text)] -> [Text] -> Text -> Text -> Text -> Text -> RpcResult Server Text
rpcRunDebS3WithCommonArgs cmd opts args bucket codename component arch =
    rpcRunDebS3 cmd ([("bucket", Just bucket), ("codename", Just codename),
                      ("component", Just component), ("arch", Just arch)] ++ opts) args

rpcRunDebS3 :: Text -> [(Text, Maybe Text)] -> [Text] -> RpcResult Server Text
rpcRunDebS3 cmd opts args = do
    (success, output, err) <- liftIO $ runDebS3 cmd opts args
    if success
        then return output
        else throwError $ rpcError debS3FailedErrorNo err

runDebS3 :: Text -> [(Text, Maybe Text)] -> [Text] -> IO (Bool, Text, Text)
runDebS3 cmd opts posArgs = do
    -- XXX: should read from handles straight into a Text, using createProcess
    (exitCode, out, err) <- readProcessWithExitCode "deb-s3" (fmap Text.unpack args) ""
    return (exitCode == ExitSuccess, Text.pack out, Text.pack err)
    where args = cmd : posArgs ++ fmap makeOpt opts
          makeOpt (k, v) = Text.concat ["--", k, case v of
                                                   Just s -> Text.append "=" s
                                                   Nothing -> Text.empty]

-- Other helpers

packageDir :: RpcResult Server FilePath
packageDir = do
    Request {reqId, tempDir} <- ask
    return $ tempDir </> (Text.unpack . TE.decodeUtf8 . B64.encode) reqId

byteStringToHex :: B.ByteString -> Text
byteStringToHex = TE.decodeUtf8 . LB.toStrict . BB.toLazyByteString . BB.byteStringHex

sanitiseFileName :: Text -> Text
sanitiseFileName = Text.filter $ not . isPathSeparator
