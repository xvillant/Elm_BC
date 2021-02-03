module Api.Article.Comment exposing (..)
import Json.Decode as D
import Api.Profile


type alias Comment =
    { id : Int
    , comment : String
    , author : Profile
    }


decoder : D.Decoder Comment
decoder =
    D.map5 Comment
        (D.field "id" D.int)
        (D.field "body" D.string)
        (D.field "author" Api.Profile.decoder)
