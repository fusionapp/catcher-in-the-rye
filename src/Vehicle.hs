module Vehicle
  ( loadVehicleData
  ) where

import App (AppM)
import Mailgun (Message)

loadVehicleData :: Message -> AppM a
loadVehicleData = error "loadVehicleData"
