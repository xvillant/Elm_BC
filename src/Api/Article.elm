module Api.Article exposing (Article, articleDecoder, articlesDecoder)
import Api.Profile exposing (Profile, profileDecoder)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline
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
    , image : String
    , userId : Int
    }


articleDecoder : Decoder Article
articleDecoder =
    D.succeed Article
        |> Json.Decode.Pipeline.required "id" D.int
        |> Json.Decode.Pipeline.required "name" D.string
        |> Json.Decode.Pipeline.required "ingredients" (D.list D.string)
        |> Json.Decode.Pipeline.required "recipe" D.string
        |> Json.Decode.Pipeline.required "profile" profileDecoder
        |> Json.Decode.Pipeline.required "created" Iso8601.decoder
        |> Json.Decode.Pipeline.required "duration" D.int
        |> Json.Decode.Pipeline.required "image" D.string
        |> Json.Decode.Pipeline.required "userId" D.int


articlesDecoder : Decoder (List Article)
articlesDecoder =
    list articleDecoder

