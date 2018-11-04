module Executor where

import Network.HTTP.Client (Manager)
import Servant.Client

execute :: Manager -> ClientM a -> IO (Either ServantError a)
execute manager client = runClientM client $ mkClientEnv manager $ BaseUrl Https "api.line.me" 443 ""

executeBit :: Manager -> ClientM a -> IO (Either ServantError a)
executeBit manager client = runClientM client $ mkClientEnv manager $ BaseUrl Https "api.bitflyer.jp" 443 ""
