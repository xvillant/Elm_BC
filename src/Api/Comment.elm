module Api.Comment exposing (Comment, commentDecoder, commentsDecoder)
import Json.Decode as D exposing (..) 
import Time
import Iso8601

type alias Comment =
    { comment : String
    , postId : Int
    , fullname : String
    , created : Time.Posix
    , userId : Int
    }


commentDecoder : Decoder Comment
commentDecoder =
    map5 Comment
        (field "comment" D.string)
        (field "postId" D.int)
        (field "fullname" D.string)
        (field "created" Iso8601.decoder)
        (field "userId" D.int)


commentsDecoder : Decoder (List Comment)
commentsDecoder =
    list commentDecoder