-- | Main API definitions.
module API
    ( serveApp
    ) where

import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)
import Network.Wai (Application)
import Servant

import App (App, AppM, Handler)
import Mailgun (MessageBody, msgAttachment, withMessage)
import Models (Payload(Payload))
import Payload (storePayload)
import PayloadTag (PayloadTag(Vehicle))

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
meadLoader = root :<|> withMessage loadData
  where root = return "M&M loader endpoint."
        loadData message = storePayload (Payload Vehicle (msgAttachment message))

-- | Serve the API as a WAI application.
serveApp :: App -> Application
serveApp app = serve api (enter (readerToHandler app) server)
