{-# OPTIONS_HADDOCK prune #-}

-- | Servant stuff for multipart/form-data, since this isn't implemented
-- upstream yet. This code is a bit hackish, and mostly taken from
-- https://github.com/haskell-servant/servant/issues/133 as is.
module Files
  ( Files
  , MultiPartData
  , MultiPartDataT
  ) where

import Data.ByteString.Lazy (ByteString)
import Network.Wai.Parse
import Servant
import Servant.Server.Internal

data Files a

type MultiPartData = ([Param], [File ByteString])
type MultiPartDataT a = (MultiPartData -> IO a) -> IO a

instance HasServer sublayout config => HasServer (Files a :> sublayout) config where
  type ServerT (Files a :> sublayout) m = MultiPartDataT a -> ServerT sublayout m
  route Proxy config subserver = WithRequest $ \request ->
    route (Proxy :: Proxy sublayout) config (addBodyCheck subserver (bodyCheck request))
    where bodyCheck request = return $ Route (parseRequestBody lbsBackEnd request >>=)
