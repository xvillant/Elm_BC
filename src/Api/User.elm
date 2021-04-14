module Api.User exposing (..)

import Iso8601
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Json.Decode.Pipeline
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
    , token : String
    }


userDecoder : Decoder User
userDecoder =
    D.succeed User
        |> Json.Decode.Pipeline.required "id" D.int
        |> Json.Decode.Pipeline.required "firstname" D.string
        |> Json.Decode.Pipeline.required "lastname" D.string
        |> Json.Decode.Pipeline.required "email" D.string
        |> Json.Decode.Pipeline.required "bio" D.string
        |> Json.Decode.Pipeline.required "image" D.string
        |> Json.Decode.Pipeline.required "password" D.string
        |> Json.Decode.Pipeline.required "created" Iso8601.decoder
        |> Json.Decode.Pipeline.optional "token" D.string ""


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
