{-# LANGUAGE DuplicateRecordFields #-}

module AppDb.User (createUser, deleteUser) where

import AppDb.Models
import qualified Hasql.Session as HasqlSession
import Rel8 (Delete (Delete, deleteWhere, from, returning, using), Expr, Insert (Insert, into, onConflict, returning, rows), OnConflict (DoNothing), Returning (NoReturning), delete, insert, lit, values, (==.))

createUser :: User Expr -> DB (Either HasqlSession.SessionError ())
createUser user =
  exec $
    insert
      Insert
        { into = userSchema,
          rows = values [user],
          onConflict = DoNothing,
          returning = NoReturning
        }

deleteUser :: UserId -> DB (Either HasqlSession.SessionError ())
deleteUser userToDeleteId =
  exec $
    delete
      Delete
        { from = userSchema,
          using = pure (),
          deleteWhere = \_ u -> userId u ==. lit userToDeleteId,
          returning = NoReturning
        }
