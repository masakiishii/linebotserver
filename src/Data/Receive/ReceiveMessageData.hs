{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Receive.ReceiveMessageData where

import Data.Aeson
import GHC.Generics

data ReceiveMessageData = ReceiveMessageData
    {
      id :: String
    , typeString :: String
    , text :: String
    } deriving (Show, Generic)

instance FromJSON ReceiveMessageData where
  parseJSON (Object o) =
    ReceiveMessageData <$> o .: "id"
               <*> o .: "type"
               <*> o .: "text"

instance ToJSON ReceiveMessageData
