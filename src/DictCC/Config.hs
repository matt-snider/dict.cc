module DictCC.Config
    (
      readConfig
    , Config(..)
    ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import System.Directory

import Options (Options(..))

-- Config
data Config = Config { defaultOptions :: Options } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { defaultOptions = Options
        { optReverse  = False
        , optLimit    = 0
        , optFromLang = "en"
        , optToLang   = "de"
        , optIsNoun   = False
        , optIsVerb   = False
        }
    }

-- Read config yaml file
readConfig :: IO Config
readConfig = do
        path <- getXdgDirectory XdgConfig ".dict-cc"
        yaml <- try $ BS.readFile path :: IO (Either SomeException ByteString)
        return $ case yaml of
            Right s  -> fromMaybe defaultConfig (Y.decode s)
            Left exc -> defaultConfig


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
        v .: "to"      <*>
        pure False     <*>
        pure False
    parseJSON _ = fail "Expected Object for Config value"

