module Data.JavaRuntime
    ( FileType (..)
    , JavaRuntime
    , getJavaRuntimeFiles
    , getJavaRuntimeFileName
    , isJavaRuntimeFileExecutable
    , getJavaRuntimeFileType
    , getLocalJavaRuntimeFilePath
    , getJavaRuntimeFileSymlinkTarget
    , getJavaRuntimeRawFileUrl
    , getJavaRuntimeRawFileSha1
    , parseJavaRuntimeManifest
    ) where

import           Data.Aeson
import           Data.Aeson.Key                       (toString)
import qualified Data.Aeson.KeyMap                    as KM
import           Data.Aeson.Types                     (parseEither)
import           Data.ByteString                      (ByteString)
import           Data.JavaRuntime.JavaRuntimeManifest (JavaRuntimeVariant)
import           Data.Maybe                           (fromJust, fromMaybe)
import           Data.Minecraft                       (MinecraftDir)
import           System.FilePath                      ((</>))
import           Text.Printf                          (printf)

data FileType = File | Directory | Link deriving (Show, Eq)

instance FromJSON FileType where
    parseJSON (String "file")      = pure File
    parseJSON (String "directory") = pure Directory
    parseJSON (String "link")      = pure Link
    parseJSON x = fail (printf "Invalid FileType structure: %s" (show x))

data FileDownload = FileDownload
    { fileDownloadUrl_  :: String
    , fileDownloadSha1_ :: String
    }
    deriving Show

instance FromJSON FileDownload where
    parseJSON (Object m) =
        FileDownload
            <$> (m .: "url")
            <*> (m .: "sha1")
    parseJSON x = fail (printf "Invalid FileDownload structure: %s" (show x))

data FileDownloads = FileDownloads
    { rawFileDownload_  :: FileDownload
    , lzmaFileDownload_ :: Maybe FileDownload
    }
    deriving Show

instance FromJSON FileDownloads where
    parseJSON (Object m) =
        FileDownloads
            <$> (m .:  "raw")
            <*> (m .:? "lzma")
    parseJSON x = fail (printf "Invalid FileDownloads structure: %s" (show x))

data JavaRuntimeFile_ = JavaRuntimeFile_
    { javaRuntimeFileDownloads__     :: Maybe FileDownloads
    , isJavaRuntimeFileExecutable__  :: Maybe Bool
    , javaRuntimeFileType__          :: FileType
    , javaRuntimeFileSymlinkTarget__ :: Maybe FilePath
    }
    deriving Show

instance FromJSON JavaRuntimeFile_ where
    parseJSON (Object m) =
        JavaRuntimeFile_
            <$> (m .:? "downloads")
            <*> (m .:? "executable")
            <*> (m .:  "type")
            <*> (m .:? "target")
    parseJSON x = fail (printf "Invalid JavaRuntimeFile structure: %s" (show x))

data JavaRuntimeFile = JavaRuntimeFile
    { javaRuntimeFileName_          :: String
    , javaRuntimeFileDownloads_     :: Maybe FileDownloads
    , isJavaRuntimeFileExecutable_  :: Maybe Bool
    , javaRuntimeFileType_          :: FileType
    , javaRuntimeFileSymlinkTarget_ :: Maybe FilePath
    }
    deriving Show

newtype JavaRuntime = JavaRuntime
    { javaRuntimeFiles_ :: [JavaRuntimeFile]
    }
    deriving Show

newtype JavaRuntime_ = JavaRuntime_
    { javaRuntimeFiles__ :: Object
    }
    deriving Show

instance FromJSON JavaRuntime_ where
    parseJSON (Object m) =
        JavaRuntime_
            <$> (m .: "files")
    parseJSON x = fail (printf "Invalid JavaRuntime structure: %s" (show x))

getJavaRuntimeFiles :: JavaRuntime -> [JavaRuntimeFile]
getJavaRuntimeFiles = javaRuntimeFiles_

getJavaRuntimeFileName :: JavaRuntimeFile -> String
getJavaRuntimeFileName = javaRuntimeFileName_

isJavaRuntimeFileExecutable :: JavaRuntimeFile -> Bool
isJavaRuntimeFileExecutable = fromMaybe False . isJavaRuntimeFileExecutable_

getJavaRuntimeFileType :: JavaRuntimeFile -> FileType
getJavaRuntimeFileType = javaRuntimeFileType_

getLocalJavaRuntimeFilePath :: MinecraftDir -> JavaRuntimeVariant -> JavaRuntimeFile -> FilePath
getLocalJavaRuntimeFilePath mcDir variant runtimeFile =
    mcDir </> "runtime" </> show variant </> getJavaRuntimeFileName runtimeFile

getJavaRuntimeFileSymlinkTarget :: JavaRuntimeFile -> FilePath
getJavaRuntimeFileSymlinkTarget = fromJust . javaRuntimeFileSymlinkTarget_

getJavaRuntimeRawFileUrl :: JavaRuntimeFile -> String
getJavaRuntimeRawFileUrl = fileDownloadUrl_ . rawFileDownload_ . fromJust . javaRuntimeFileDownloads_

getJavaRuntimeRawFileSha1 :: JavaRuntimeFile -> String
getJavaRuntimeRawFileSha1 = fileDownloadSha1_ . rawFileDownload_ . fromJust . javaRuntimeFileDownloads_

parseJavaRuntimeManifest :: ByteString -> Either String JavaRuntime
parseJavaRuntimeManifest rawJson =
    case eitherDecodeStrict' rawJson of
        Right javaRuntime_ ->
            let eitherRuntimeFiles = flip parseEither (KM.toList (javaRuntimeFiles__ javaRuntime_)) $
                    mapM
                        (\(key, val) -> do
                            obj <- parseJSON val

                            return $
                                JavaRuntimeFile
                                    { javaRuntimeFileName_          = toString key
                                    , javaRuntimeFileDownloads_     = javaRuntimeFileDownloads__ obj
                                    , isJavaRuntimeFileExecutable_  = isJavaRuntimeFileExecutable__ obj
                                    , javaRuntimeFileType_          = javaRuntimeFileType__ obj
                                    , javaRuntimeFileSymlinkTarget_ = javaRuntimeFileSymlinkTarget__ obj
                                    }
                        )
            in
            case eitherRuntimeFiles of
                Right runtimeFiles ->
                    Right $
                        JavaRuntime
                            { javaRuntimeFiles_ = runtimeFiles
                            }

                Left err ->
                    Left err

        Left err ->
            Left err
