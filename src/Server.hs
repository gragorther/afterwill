{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server
  (
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Servant
import Servant.API

type AppRespType = JSON

type AppReqBody = ReqBody '[AppRespType]

type AppGet = Get '[AppRespType]

-- to allow changing the type the app uses for various IDs in the future
type ID = Int

data RegistrationUser = RegistrationUser
  { ruserUsername :: Text,
    ruserEmail :: Text,
    ruserPassword :: Text,
    --   userId :: Int64,
    ruserName :: Maybe Text
  }

data LoginUser = LoginUser
  { luserUsername :: Text,
    luserPassword :: Text
  }

type UserAPI =
  "register" :> ReqBody '[AppRespType] RegistrationUser :> PostNoContent -- TODO: use actual response type here (not NoContent)
    :<|> "login" :> ReqBody '[AppRespType] LoginUser :> PostNoContent

type CRUDAPI create read =
  ReqBody '[AppRespType] create :> PostNoContent
    :<|> Capture "id" ID :> AppGet read
    :<|> Capture "id" ID :> ReqBody '[AppRespType] create :> PatchNoContent -- there is no `update` content type here because it'd be the same as create in most cases
    :<|> Capture "id" ID :> AppReqBody ID :> Delete '[JSON] NoContent -- should this be NoContent? there might be a better way to do it