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
import PayloadTag (PayloadTag(..))

type AppServer api = ServerT api AppM

type SimpleLoader = Get '[PlainText] Text
                    :<|> MessageBody :> Post '[JSON] ()

type API = "mg" :> (
  Get '[PlainText] Text
  :<|> "mead" :> SimpleLoader
  :<|> "motorite" :> SimpleLoader
  )

api :: Proxy API
api = Proxy

readerToHandler :: App -> AppM :~> Handler
readerToHandler app = Nat $ \x -> runReaderT x app

server :: AppServer API
server = root
         :<|> simpleLoader "M&M loader endpoint." Vehicle
         :<|> simpleLoader "Motorite loader endpoint." Motorite
  where root = return "Mailgun service endpoint."

simpleLoader :: Text -> PayloadTag -> AppServer SimpleLoader
simpleLoader desc tag = root :<|> withMessage loadData
  where root = return desc
        loadData message = storePayload (Payload tag (msgAttachment message))

-- | Serve the API as a WAI application.
serveApp :: App -> Application
serveApp app = serve api (enter (readerToHandler app) server)
