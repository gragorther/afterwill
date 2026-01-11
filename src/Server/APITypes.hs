{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Server.APITypes
  ( Message (..),
    Group (..),
    Recipient (..),
    MessageAPI,
    GroupAPI,
    RecipientAPI,
  )
where

import Data.Text (Text)
import Servant

type AppRespType = JSON

type AppReqBody = ReqBody '[AppRespType]

type AppGet = Get '[AppRespType]

type AppDelete = Delete '[AppRespType]

type AppPost = Post '[AppRespType]

-- to allow changing the type the app uses for various IDs in the future
type ID = Int

type CRUDAPI create read =
  AppReqBody create :> PostNoContent -- create
    :<|> Capture "id" ID
      :> ( AppGet read -- read
             :<|> AppReqBody create :> PatchNoContent -- update
             :<|> AppReqBody ID :> DeleteNoContent -- delete
         )
    :<|> AppGet [read] -- read all

data Message = Message
  { title :: Text,
    content :: Text
  }

type MessageAPI = CRUDAPI Message Message

data Group = Group
  { name :: Text,
    description :: Text
  }

type GroupAPI = CRUDAPI Group Group

newtype Recipient = Recipient
  { email :: Text
  }

type RecipientAPI = CRUDAPI Recipient Recipient