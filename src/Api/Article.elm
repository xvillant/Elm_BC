module Api.Article exposing (Article, articleDecoder, articlesDecoder)
import Api.Profile exposing (Profile, profileDecoder)
import Json.Decode as D exposing (..)
import Time
import Iso8601

type alias Article =
    { id : Int 
    , name : String
    , ingredients : List String
    , recipe : String
    , profile : Profile
    , created : Time.Posix
    , duration : Int
    }


articleDecoder : Decoder Article
articleDecoder =
    map7 Article
        (field "id" D.int)
        (field "name" D.string)
        (field "ingredients" (D.list D.string))
        (field "recipe" D.string)
        (field "profile" profileDecoder)
        (field "created" Iso8601.decoder)
        (field "duration" D.int)


articlesDecoder : Decoder (List Article)
articlesDecoder =
    list articleDecoder

