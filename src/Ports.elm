port module Ports exposing (clearUser, saveUser)

import Api.User exposing (User, userEncode)
import Json.Decode as D
import Json.Encode as E


port outgoing :
    { tag : String
    , data : D.Value
    }
    -> Cmd msg


saveUser : User -> Cmd msg
saveUser user =
    outgoing
        { tag = "saveUser"
        , data = userEncode user
        }


clearUser : Cmd msg
clearUser =
    outgoing
        { tag = "clearUser"
        , data = E.null
        }



--https://github.com/ryannhg/elm-spa-realworld
