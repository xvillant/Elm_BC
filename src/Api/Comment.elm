module Api.Comment exposing (Comment, commentDecoder, commentsDecoder)
import Api.Profile exposing (Profile, profileDecoder)
import Json.Decode as D exposing (..) 
import Iso8601
import Time

type alias Comment =
    { userid : Int
    , comment : String
    , recipeid : Int
    , profile : Profile
    , created : Time.Posix
    }


commentDecoder : Decoder Comment
commentDecoder =
    map5 Comment
        (field "userid" D.int)
        (field "comment" D.string)
        (field "recipeid" D.int)
        (field "profile" profileDecoder)
        (field "created" Iso8601.decoder)


commentsDecoder : Decoder (List Comment)
commentsDecoder =
    list commentDecoder