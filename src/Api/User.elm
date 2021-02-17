module Api.User exposing (..)

import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)

type alias User =
    { firstname : String
    , lastname : String
    , token : String
    , email : String
    , bio : String
    }

decoder : Decoder User
decoder =
    map5 User
        (field "firstname" D.string)
        (field "lastname" D.string)
        (field "email" D.string)
        (field "bio" D.string)
        (field "token" D.string)

encode : User -> E.Value
encode user =
    E.object
        [ ( "firstname", E.string user.firstname )
        , ( "lastname", E.string user.lastname)
        , ( "email", E.string user.email )
        , ( "bio", E.string user.bio )
        ]