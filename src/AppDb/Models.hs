{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AppDb.Models (exec, User (..), userSchema, Message (..), messageSchema, Recipient (..), recipientSchema, Group (..), groupSchema, MessageGroup (..), mgSchema, DB, UserId, MessageId, RecipientId) where

import Control.Monad.Reader
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import qualified Hasql.Session as HasqlSession
import Rel8

type DB = ReaderT Connection IO

exec :: Statement exprs -> DB (Either HasqlSession.SessionError ())
exec stmt = ReaderT $ \conn -> do
  let hasqlStmt = run_ stmt
  HasqlSession.run (HasqlSession.statement () hasqlStmt) conn

newtype UserId = UserId Int64 deriving newtype (DBEq, DBType, Eq, Show)

data User f = User
  { userId :: Column f UserId,
    userName :: Column f (Maybe Text),
    userEmail :: Column f Text,
    userPasswordHash :: Column f (Maybe Text),
    userUsername :: Column f Text
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
            userPasswordHash = "password_hash",
            userUsername = "username"
          }
    }

newtype MessageId = MessageId Int64 deriving newtype (DBEq, DBType, Eq, Show)

data Message f = Message
  { messageId :: Column f MessageId,
    messageContent :: Column f Text,
    messageTitle :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

messageSchema :: TableSchema (Message Name)
messageSchema =
  TableSchema
    { name = "messages",
      columns =
        Message
          { messageId = "id",
            messageContent = "content",
            messageTitle = "title"
          }
    }

newtype GroupId = GroupId Int64 deriving newtype (DBEq, DBType, Eq, Show)

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

mgSchema :: TableSchema (MessageGroup Name)
mgSchema =
  TableSchema
    { name = "messages_groups",
      columns =
        MessageGroup
          { mgMessageId = "message_id",
            mgGroupId = "group_id"
          }
    }

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