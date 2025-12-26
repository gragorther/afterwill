{-# LANGUAGE DuplicateRecordFields #-}

module AppDb.Message (createMessage, deleteMessage) where

import AppDb.Models
import Hasql.Session
import Rel8

createMessage :: Message Expr -> DB (Either SessionError ())
createMessage msg =
  exec $
    insert
      Insert
        { into = messageSchema,
          rows = values [msg],
          onConflict = DoNothing,
          returning = NoReturning
        }

deleteMessage :: MessageId -> DB (Either SessionError ())
deleteMessage msgid =
  exec $
    delete
      Delete
        { from = messageSchema,
          using = pure (),
          deleteWhere = \_ msg -> messageId msg ==. lit msgid,
          returning = NoReturning
        }
