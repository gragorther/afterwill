{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- for toDbMessage:
{-# OPTIONS_GHC -Wno-missing-fields #-}

module AppDb.Message (createMessage, deleteMessage, messageById, Message (..)) where

import AppDb.Models
import Data.Text (Text)
import Rel8

data Message = Message
  { title :: Text,
    content :: Text,
    userId :: UserId,
    groupIds :: Maybe [GroupId]
  }

toDbMessage :: Message -> DbMessage Result
toDbMessage msg =
  DbMessage
    { content = msg.content,
      title = msg.title,
      userId = msg.userId
    }

fromDbMessage :: DbMessage Result -> Message
fromDbMessage msg =
  Message
    { title = msg.title,
      content = msg.content,
      userId = msg.userId
    }

createMessage :: Message -> DB ()
createMessage msg =
  exec $
    insert
      Insert
        { into = messageSchema,
          rows =
            values
              [ lit $ toDbMessage msg
              ],
          onConflict = DoNothing,
          returning = NoReturning
        }

deleteMessage :: MessageId -> DB ()
deleteMessage msgid =
  exec $
    delete
      Delete
        { from = messageSchema,
          using = pure (),
          deleteWhere = \_ msg -> msg.id ==. lit msgid,
          returning = NoReturning
        }

messageById :: MessageId -> DB (Maybe Message)
messageById mid =
  fmap
    (fmap fromDbMessage)
    ( queryMaybe $ do
        msg <- each messageSchema
        where_ $ msg.id ==. lit mid
        return msg
    )
