module Api.User exposing (..)

import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)


type alias User =
    { id : Int
    , firstname : String
    , lastname : String
    , email : String
    , bio : String
    , image : String
    , password : String
    }


decoder : Decoder User
decoder =
    map7 User
        (field "id" D.int)
        (field "firstname" D.string)
        (field "lastname" D.string)
        (field "email" D.string)
        (field "bio" D.string)
        (field "image" D.string)
        (field "password" D.string)


encode : User -> E.Value
encode user =
    E.object
        [ ( "id", E.int user.id)
        , ( "firstname", E.string user.firstname )
        , ( "lastname", E.string user.lastname )
        , ( "email", E.string user.email )
        , ( "bio", E.string user.bio )
        , ( "image", E.string user.image)
        ]
