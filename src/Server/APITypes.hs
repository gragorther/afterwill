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
    API,
  )
where

import Data.Text (Text)
import GHC.Natural (Natural)
import Servant
import Servant.Auth (Auth)

type AppRespType = JSON

type AppReqBody = ReqBody '[AppRespType]

type AppGet = Get '[AppRespType]

type AppDelete = Delete '[AppRespType]

type AppPost = Post '[AppRespType]

-- to allow changing the type the app uses for various IDs in the future
type ID = Natural

type Protected = MessageAPI :<|> GroupAPI :<|> RecipientAPI

type API auths = (Servant.Auth.Auth auths ID :> Protected)

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