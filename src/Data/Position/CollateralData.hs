{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Position.CollateralData where

import Data.Aeson
import GHC.Generics

data CollateralData = CollateralData
    {
      collateral :: Integer
    , openPositionPnL :: Integer
    , requireCollateral :: Integer
    , keepRate :: Double
    } deriving (Show, Generic)

instance FromJSON CollateralData where
  parseJSON (Object o) =
    CollateralData <$> o .: "collateral"
                   <*> o .: "open_position_pnl"
                   <*> o .: "require_collateral"
                   <*> o .: "keep_rate"

instance ToJSON CollateralData where
  toJSON v = object ["collateral" .= collateral v,
                     "open_position_pnl" .= openPositionPnL v,
                     "require_collateral" .= requireCollateral v,
                     "keep_rate" .= keepRate v]

