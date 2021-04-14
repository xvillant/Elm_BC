module Api.Comment exposing (Comment, commentDecoder, commentsDecoder)
import Api.Profile exposing (Profile, profileDecoder)
import Json.Decode as D exposing (..) 
import Time
import Iso8601

type alias Comment =
    { comment : String
    , recipeid : Int
    , profile : Profile
    , created : Time.Posix
    , userId : Int
    }


commentDecoder : Decoder Comment
commentDecoder =
    map5 Comment
        (field "comment" D.string)
        (field "recipeid" D.int)
        (field "profile" profileDecoder)
        (field "created" Iso8601.decoder)
        (field "userId" D.int)


commentsDecoder : Decoder (List Comment)
commentsDecoder =
    list commentDecoder