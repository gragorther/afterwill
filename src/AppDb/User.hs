{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module AppDb.User (createUser, deleteUser, userById) where

import AppDb.Models
import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8

data User = User
  { username :: Text,
    email :: Text,
    id :: UserId,
    name :: Maybe Text,
    passwordHash :: Text
  }
  deriving (Generic)

toDbUser :: User -> DbUser Result
toDbUser usr =
  DbUser
    { username = usr.username,
      email = usr.email,
      passwordHash = usr.passwordHash,
      name = usr.name,
      id = usr.id
    }

fromDbUser :: DbUser Result -> User
fromDbUser usr =
  User
    { username = usr.username
    }

createUser :: User -> DB ()
createUser user =
  exec $
    insert
      Insert
        { into = userSchema,
          rows = values [lit $ toDbUser user],
          onConflict = DoNothing,
          returning = NoReturning
        }

deleteUser :: UserId -> DB ()
deleteUser userToDeleteId =
  exec $
    delete
      Delete
        { from = userSchema,
          using = pure (),
          deleteWhere = \_ u -> u.id ==. lit userToDeleteId,
          returning = NoReturning
        }

userById :: UserId -> DB (Maybe User)
userById uid =
  fmap
    (fmap fromDbUser)
    ( queryMaybe $ do
        user <- each userSchema
        where_ $ user.id ==. lit uid
        return user
    )
