{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Send.SendMessageData where

import Data.Aeson
import GHC.Generics

data SendMessageData = SendMessageData
    {
      typeString :: String
    , text :: String
    } deriving (Show, Generic)

instance FromJSON SendMessageData where
  parseJSON (Object o) =
    SendMessageData <$> o .: "type"
                    <*> o .: "text"

instance ToJSON SendMessageData where
  toJSON v = object ["type" .= typeString v,
                     "text" .= text v]

