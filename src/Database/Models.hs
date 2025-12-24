{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Models (User (..), userSchema, Message (..), messageSchema, Recipient (..), recipientSchema, Group (..), groupSchema) where

import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8

newtype UserId = UserId Int64 deriving newtype (DBEq, DBType, Eq, Show)

data User f = User
  { userId :: Column f UserId,
    userName :: Column f String,
    userEmail :: Column f String,
    userPasswordHash :: Column f String
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

userSchema :: TableSchema (User Name)
userSchema =
  TableSchema
    { name = "users",
      --     schema = Nothing,
      columns =
        User
          { userId = "id",
            userName = "name",
            userEmail = "email",
            userPasswordHash = "password_hash"
          }
    }

newtype MessageId = MessageId Int64 deriving newtype (DBEq, DBType, Eq, Show)

data Message f = Message
  { messageId :: Column f MessageId,
    messageContent :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

messageSchema :: TableSchema (Message Name)
messageSchema =
  TableSchema
    { name = "users",
      columns =
        Message
          { messageId = "id",
            messageContent = "content"
          }
    }

newtype GroupId = GroupID Int64 deriving newtype (DBEq, DBType, Eq, Show)

data Group f = Group
  { groupName :: Column f Text,
    groupDescription :: Column f (Maybe Text),
    groupId :: Column f GroupId,
    groupUserId :: Column f UserId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

groupSchema :: TableSchema (Group Name)
groupSchema =
  TableSchema
    { name = "groups",
      columns =
        Group
          { groupName = "name",
            groupDescription = "description",
            groupId = "id",
            groupUserId = "user_id"
          }
    }

data MessageGroup f = MessageGroup
  { mgMessageId :: Column f MessageId,
    mgGroupId :: Column f GroupId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

newtype RecipientId = RecipientId Int64 deriving newtype (DBEq, DBType, Eq, Show)

data Recipient f = Recipient
  {recipientEmail :: Column f Text, recipientGroupId :: Column f GroupId, recipientId :: Column f RecipientId}

recipientSchema :: TableSchema (Recipient Name)
recipientSchema =
  TableSchema
    { name = "recipients",
      columns =
        Recipient
          { recipientEmail = "email",
            recipientGroupId = "group_id",
            recipientId = "id"
          }
    }