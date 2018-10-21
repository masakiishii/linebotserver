{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Receive.EventData where

import Data.Aeson
import GHC.Generics

import Data.Receive.SourceData
import Data.Receive.ReceiveMessageData

data EventData = EventData
    {
      replyToken :: String
    , typeString :: String
    , timestamp :: Integer
    , source :: SourceData
    , message :: Maybe ReceiveMessageData
    } deriving (Show, Generic)

instance FromJSON EventData where
  parseJSON (Object o) =
    EventData <$> o .: "replyToken"
               <*> o .: "type"
               <*> o .: "timestamp"
               <*> o .: "source"
               <*> o .:? "message"

instance ToJSON EventData
