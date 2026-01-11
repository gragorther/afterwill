{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AppDb.Models (query, query1, queryMaybe, exec, DbUser (..), userSchema, DbMessage (..), messageSchema, DbRecipient (..), recipientSchema, DbGroup (..), groupSchema, MessageGroup (..), mgSchema, DB, UserId, MessageId, RecipientId, GroupId) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import qualified Hasql.Session as HasqlSession
import Rel8

type DB = ReaderT Connection (ExceptT HasqlSession.SessionError IO)

exec :: Statement exprs -> DB ()
exec stmt = do
  conn <- ask
  let hasqlStmt = run_ stmt
  lift $ ExceptT $ HasqlSession.run (HasqlSession.statement () hasqlStmt) conn

query :: (Serializable exprs a) => Query exprs -> DB [a]
query q = do
  conn <- ask
  let hasqlStmt = run $ select q
  lift $ ExceptT $ HasqlSession.run (HasqlSession.statement () hasqlStmt) conn

query1 :: (Serializable exprs a) => Query exprs -> DB a
query1 q = do
  conn <- ask
  let hasqlStmt = run1 $ select q
  lift $ ExceptT $ HasqlSession.run (HasqlSession.statement () hasqlStmt) conn

queryMaybe :: (Serializable exprs a) => Query exprs -> DB (Maybe a)
queryMaybe q = do
  conn <- ask
  let hasqlStmt = runMaybe $ select q
  lift $ ExceptT $ HasqlSession.run (HasqlSession.statement () hasqlStmt) conn

newtype UserId = UserId Int64 deriving newtype (DBEq, DBType, Eq, Show)

data DbUser f = DbUser
  { id :: Column f UserId,
    name :: Column f (Maybe Text),
    email :: Column f Text,
    passwordHash :: Column f Text,
    username :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

userSchema :: TableSchema (DbUser Name)
userSchema =
  TableSchema
    { name = "users",
      --     schema = Nothing,
      columns =
        DbUser
          { id = "id",
            name = "name",
            email = "email",
            passwordHash = "password_hash",
            username = "username"
          }
    }

newtype MessageId = MessageId Int64 deriving newtype (DBEq, DBType, Eq, Show)

data DbMessage f = DbMessage
  { id :: Column f MessageId,
    content :: Column f Text,
    title :: Column f Text,
    userId :: Column f UserId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

messageSchema :: TableSchema (DbMessage Name)
messageSchema =
  TableSchema
    { name = "messages",
      columns =
        DbMessage
          { id = "id",
            content = "content",
            title = "title",
            userId = "user_id"
          }
    }

newtype GroupId = GroupId Int64 deriving newtype (DBEq, DBType, Eq, Show)

data DbGroup f = DbGroup
  { name :: Column f Text,
    description :: Column f (Maybe Text),
    id :: Column f GroupId,
    userId :: Column f UserId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

groupSchema :: TableSchema (DbGroup Name)
groupSchema =
  TableSchema
    { name = "groups",
      columns =
        DbGroup
          { name = "name",
            description = "description",
            id = "id",
            userId = "user_id"
          }
    }

data MessageGroup f = MessageGroup
  { messageId :: Column f MessageId,
    groupId :: Column f GroupId
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

mgSchema :: TableSchema (MessageGroup Name)
mgSchema =
  TableSchema
    { name = "messages_groups",
      columns =
        MessageGroup
          { messageId = "message_id",
            groupId = "group_id"
          }
    }

newtype RecipientId = RecipientId Int64 deriving newtype (DBEq, DBType, Eq, Show)

data DbRecipient f = DbRecipient
  {email :: Column f Text, groupId :: Column f GroupId, id :: Column f RecipientId}

recipientSchema :: TableSchema (DbRecipient Name)
recipientSchema =
  TableSchema
    { name = "recipients",
      columns =
        DbRecipient
          { email = "email",
            groupId = "group_id",
            id = "id"
          }
    }