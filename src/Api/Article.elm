module Api.Article exposing (Article, decoder)
import Api.Profile exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)

type alias Article =
    { id : Int 
    , name : String
    , ingredients : List String
    , recipe : String
    , profile : Api.Profile.Profile
    }


decoder : Decoder Article
decoder =
    map5 Article
        (field "id" D.int)
        (field "name" D.string)
        (field "ingredients" (D.list D.string))
        (field "recipe" D.string)
        (field "profile" Api.Profile.decoder)

