{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

module SlackTypes where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data SlackMessage 
 = SlackMessage
   { slackMsgText :: T.Text
   , slackMsgAttachments :: Maybe [Attachment]
   }
 deriving (Show, Generic)

data Attachment
 = Attachment
   { attachmentTitle :: Maybe T.Text
   , attachmentText :: Maybe T.Text
   , attachmentImgUrl :: Maybe T.Text
   }
 deriving (Show, Generic)

instance ToJSON Attachment
instance FromJSON Attachment
instance ToJSON SlackMessage
instance FromJSON SlackMessage

basicMessage = flip SlackMessage Nothing

basicAttachment = Attachment Nothing . Just

imgAttachment = Attachment Nothing Nothing . Just

infixl 5 +:
(+:) :: SlackMessage -> Attachment -> SlackMessage
(+:) SlackMessage{..} attachment =
  case slackMsgAttachments of
    Nothing -> msg $ Just [attachment]
    Just as -> msg (Just $ attachment : as)
  where msg = SlackMessage slackMsgText

