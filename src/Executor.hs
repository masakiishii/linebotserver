module Executor where

import Network.HTTP.Client (Manager)
import Servant.Client

-- LineMessage executor
execute :: Manager -> ClientM a -> IO (Either ServantError a)
execute manager client = runClientM client $ mkClientEnv manager $ BaseUrl Https "api.line.me" 443 ""
