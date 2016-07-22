module Types where

import Data.Aeson
import Data.Text (Text, pack)
import Data.Map (Map, fromList)

data SlackValue = SText Text
                | SMap (Map Text SlackValue)
                | SList [SlackValue]
                deriving (Show)

instance ToJSON SlackValue where
  toJSON (SText t) = toJSON t
  toJSON (SList l) = toJSON l
  toJSON (SMap m) = toJSON m

stringToSlack = SText . pack

slackPacket = SMap . fromList
