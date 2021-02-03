module Api.User exposing (..)

import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)

type alias User =
    { username : String
    , token : String
    , email : String
    , bio : String
    }

decoder : Decoder User
decoder =
    map4 User
        (field "username" D.string)
        (field "email" D.string)
        (field "bio" D.string)
        (field "token" D.string)

encode : User -> E.Value
encode user =
    E.object
        [ ( "username", E.string user.username )
        , ( "email", E.string user.email )
        , ( "bio", E.string user.bio )
        ]