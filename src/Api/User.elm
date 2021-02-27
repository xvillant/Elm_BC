module Api.User exposing (..)

import Iso8601
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Time


type alias User =
    { id : Int
    , firstname : String
    , lastname : String
    , email : String
    , bio : String
    , image : String
    , password : String
    , created : Time.Posix
    }


userDecoder : Decoder User
userDecoder =
    map8 User
        (field "id" D.int)
        (field "firstname" D.string)
        (field "lastname" D.string)
        (field "email" D.string)
        (field "bio" D.string)
        (field "image" D.string)
        (field "password" D.string)
        (field "created" Iso8601.decoder)


userEncode : User -> E.Value
userEncode user =
    E.object
        [ ( "id", E.int user.id )
        , ( "email", E.string user.email )
        , ( "firstname", E.string user.firstname )
        , ( "lastname", E.string user.lastname )
        , ( "bio", E.string user.bio )
        , ( "image", E.string user.image )
        , ( "created", Iso8601.encode user.created )
        , ( "password", E.string user.password)
        ]
