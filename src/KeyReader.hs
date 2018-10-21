{-# LANGUAGE OverloadedStrings #-}

module KeyReader where

import GHC.Generics
import System.IO
import Data.Csv
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

data AccessKeyData = AccessKeyData
    {
        accessToken :: !String
      , channelSecret :: !String
    }

instance FromNamedRecord AccessKeyData where
    parseNamedRecord r = AccessKeyData <$> r .: "accessToken" <*> r .: "channelSecret"

readKeyData :: BL.ByteString -> [String]
readKeyData csvData =
    case decodeByName csvData of
        Left err -> [""]
        Right (_, v) -> [accessToken $ V.head v ,channelSecret $ V.head v]
