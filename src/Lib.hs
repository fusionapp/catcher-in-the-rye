module Lib
    ( app
    , startApp
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text)
import Data.Thyme.Clock (getCurrentTime)
import Mailgun (APIKey, UnverifiedMessage, verifySignature)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type Handler = ExceptT ServantErr IO

type MeadLoader = Get '[PlainText] Text
                  :<|> ReqBody '[JSON] UnverifiedMessage :> Post '[JSON] ()


type API = "mg" :> (
  Get '[PlainText] Text
  :<|> "mead" :> MeadLoader
  )

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = root :<|> meadLoader
  where root = return "Mailgun service endpoint."

errSignature :: ServantErr
errSignature = err403 { errBody = "Invalid signature." }

key :: APIKey
key = undefined

meadLoader :: Server MeadLoader
meadLoader = root :<|> loadData
  where root :: Handler Text
        root = return "M&M loader endpoint."
        loadData :: UnverifiedMessage -> Handler ()
        loadData message' = do
          now <- liftIO getCurrentTime
          case verifySignature key now message' of
            Just message -> return ()
            Nothing -> throwE errSignature
