-- | Delivery of upload notifications.
module Notifications
  ( NotificationTarget(..)
  , Notification(..)
  , deliverNotifications
  , mkSlackNotification
  ) where

import           Control.Exception.Lens (catching, _IOException)
import           Control.Monad (void)
import           Data.Aeson.TH
import           Data.Default (def)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Network.URI (parseAbsoluteURI, uriAuthority, uriRegName)
import           Network.Wreq (post)
import           System.IO (hPrint, stderr)

import           PayloadTag (PayloadTag)
import           Rules (jsonOptions)
import           Slack (SlackMessage(..), SlackAttachment(..), SlackField(..))

-- | The target of a notification.
data NotificationTarget = Slack { slackUrl :: String }
                        -- ^ Notify Slack via the webhook interface.
  deriving (Show, Eq)

deriveJSON jsonOptions ''NotificationTarget

-- | The notification to be delivered.
data Notification = Success
                    { tag :: PayloadTag
                    -- ^ The type of payload being uploaded.
                    , destination :: String
                    -- ^ The upload destination.
                    }
                  -- ^ An upload succeeded.
                  | Failure
                    { tag :: PayloadTag
                    -- ^ The type of payload being uploaded.
                    , destination :: String
                    -- ^ The upload destination.
                    , reason :: String
                    -- ^ A description of why the upload failed.
                    }
                  -- ^ An upload failed.

-- | Deliver a notification of the upload result to a notification target.
--
-- Any failures during notification delivery are suppressed.
deliverNotifications :: [Notification] -> NotificationTarget -> IO ()
deliverNotifications n nt = catching _IOException (deliver n nt) (hPrint stderr)
  where deliver notifications (Slack u) = void $ post u (mkSlackNotification notifications)

-- | Construct a notification message payload for Slack.
mkSlackNotification :: [Notification] -> SlackMessage
mkSlackNotification ns = def { messageAttachments = mkAttachment <$> ns }
  where mkAttachment Success {..} = def
          { attachmentFallback = "Successful upload to: " <> hostname destination
          , attachmentTitle = "Successful upload"
          , attachmentColor = Just "#00a651"
          , attachmentFields =
            [ SlackField "Payload" (T.pack (show tag)) True
            , SlackField "Destination" (hostname destination) True
            ]
          }
        mkAttachment Failure {..} = def
          { attachmentFallback = "Failed upload to: " <> hostname destination
          , attachmentTitle = "Failed upload"
          , attachmentColor = Just "#ed1c24"
          , attachmentFields =
            [ SlackField "Payload" (T.pack (show tag)) True
            , SlackField "Destination" (hostname destination) True
            , SlackField "Reason" (T.pack reason) False
            ]
          }
        hostname uri = T.pack . fromMaybe "<unknown>" $ uriRegName <$> (uriAuthority =<< parseAbsoluteURI uri)
