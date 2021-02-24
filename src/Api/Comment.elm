module Api.Comment exposing (Comment, commentDecoder, commentsDecoder)
import Api.Profile exposing (Profile, profileDecoder)
import Json.Decode as D exposing (..) 

type alias Comment =
    { comment : String
    , recipeid : Int
    , profile : Profile
    }


commentDecoder : Decoder Comment
commentDecoder =
    map3 Comment
        (field "comment" D.string)
        (field "recipeid" D.int)
        (field "profile" profileDecoder)


commentsDecoder : Decoder (List Comment)
commentsDecoder =
    list commentDecoder