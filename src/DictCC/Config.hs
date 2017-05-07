{-# LANGUAGE OverloadedStrings #-}

module DictCC.Config
    (
      readConfig
    ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import System.Directory

import Options (Options(..))

-- Config
data Config = Config { defaultOptions :: Options } deriving (Show)

-- Read config yaml file
readConfig :: IO (Maybe Config)
readConfig = do
        path <- getXdgDirectory XdgConfig ".dict-cc"
        yaml <- try $ BS.readFile path :: IO (Either SomeException ByteString)
        return $ case yaml of
            Right s  -> Y.decode s :: Maybe Config
            Left exc -> Nothing


instance FromJSON Config where
    parseJSON (Y.Object v) =
        Config <$>
        v .: "defaults"
    parseJSON _ = fail "Expected Object for Config value"

instance FromJSON Options where
    parseJSON (Y.Object v) =
        Options <$>
        pure False <*>
        v .: "limit"   <*>
        v .: "from"    <*>
        v .: "to"
    parseJSON _ = fail "Expected Object for Config value"

