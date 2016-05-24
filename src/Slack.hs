-- | Bindings for the Slack incoming webhook API.
module Slack
  ( SlackMessage(..)
  , SlackAttachment(..)
  , SlackField(..)
  ) where

import Data.Aeson (toJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Default (Default(..))
import Data.Text (Text)
import Network.Wreq.Types (Postable(..))

import Rules (slackOptions)

-- | A field in a message attachment.
data SlackField = SlackField
  { fieldTitle :: Text
  , fieldValue :: Text
  , fieldShort :: Bool
  } deriving (Show, Eq)

deriveJSON slackOptions ''SlackField

-- | A Slack message attachment.
data SlackAttachment = SlackAttachment
  { attachmentFallback :: Text
  , attachmentColor :: Maybe Text
  , attachmentPretext :: Maybe Text
  , attachmentAuthorName :: Maybe Text
  , attachmentAuthorLink :: Maybe Text
  , attachmentAuthorIcon :: Maybe Text
  , attachmentTitle :: Text
  , attachmentTitleLink :: Maybe Text
  , attachmentText :: Maybe Text
  , attachmentFields :: [SlackField]
  } deriving (Show, Eq)

deriveJSON slackOptions ''SlackAttachment

instance Default SlackAttachment where
  def = SlackAttachment
        { attachmentFallback = ""
        , attachmentColor = Nothing
        , attachmentPretext = Nothing
        , attachmentAuthorName = Nothing
        , attachmentAuthorLink = Nothing
        , attachmentAuthorIcon = Nothing
        , attachmentTitle = ""
        , attachmentTitleLink = Nothing
        , attachmentText = Nothing
        , attachmentFields = []
        }

-- | A Slack message.
data SlackMessage = SlackMessage
  { messageText :: Maybe Text
  , messageUsername :: Maybe Text
  , messageIconEmoji :: Maybe Text
  , messageChannel :: Maybe Text
  , messageAttachments :: [SlackAttachment]
  } deriving (Show, Eq)

deriveJSON slackOptions ''SlackMessage

instance Default SlackMessage where
  def = SlackMessage
        { messageText = Nothing
        , messageUsername = Nothing
        , messageIconEmoji = Nothing
        , messageChannel = Nothing
        , messageAttachments = []
        }

instance Postable SlackMessage where
  postPayload = postPayload . toJSON
