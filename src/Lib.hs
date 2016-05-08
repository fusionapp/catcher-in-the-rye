module Lib
    ( serveApp
    ) where

import App (App, AppM, Handler)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)
import Mailgun (MessageBody, withMessage)
import Network.Wai (Application)
import Servant
import Vehicle (loadVehicleData)

type AppServer api = ServerT api AppM

type MeadLoader = Get '[PlainText] Text
                  :<|> MessageBody :> Post '[JSON] ()

type API = "mg" :> (
  Get '[PlainText] Text
  :<|> "mead" :> MeadLoader
  )

api :: Proxy API
api = Proxy

readerToHandler :: App -> AppM :~> Handler
readerToHandler app = Nat $ \x -> runReaderT x app

server :: AppServer API
server = root :<|> meadLoader
  where root = return "Mailgun service endpoint."

meadLoader :: AppServer MeadLoader
meadLoader = root :<|> loadData
  where root = return "M&M loader endpoint."
        loadData = withMessage loadVehicleData

serveApp :: App -> Application
serveApp app = serve api (enter (readerToHandler app) server)
