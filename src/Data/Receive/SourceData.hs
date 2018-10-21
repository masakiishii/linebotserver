{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Receive.SourceData where

import Data.Aeson
import GHC.Generics

data SourceData = SourceData
    {
      typeString :: String
    , groupId :: Maybe String
    , roomId :: Maybe String
    , userId :: String
    } deriving (Show, Generic)

instance FromJSON SourceData where
  parseJSON (Object o) =
    SourceData <$> o .: "type"
               <*> o .:? "groupId"
               <*> o .:? "roomId"
               <*> o .: "userId"

instance ToJSON SourceData
