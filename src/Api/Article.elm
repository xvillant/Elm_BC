module Api.Article exposing (Article, articleDecoder, articlesDecoder)
import Api.Profile exposing (Profile, profileDecoder)
import Json.Decode as D exposing (..)

type alias Article =
    { id : Int 
    , name : String
    , ingredients : List String
    , recipe : String
    , profile : Profile
    }


articleDecoder : Decoder Article
articleDecoder =
    map5 Article
        (field "id" D.int)
        (field "name" D.string)
        (field "ingredients" (D.list D.string))
        (field "recipe" D.string)
        (field "profile" profileDecoder)


articlesDecoder : Decoder (List Article)
articlesDecoder =
    list articleDecoder

