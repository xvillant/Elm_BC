module Api.Profile exposing (Profile, profileDecoder, profilesDecoder)
import Json.Decode as D exposing (..)
import Time
import Iso8601


type alias Profile =
    { id : Int
    , firstname : String
    , lastname : String
    , email : String
    , bio : String
    , image : String
    , created : Time.Posix
    }

profileDecoder : Decoder Profile
profileDecoder =
    map7 Profile
        (field "id" D.int)
        (field "firstname" D.string)
        (field "lastname" D.string)
        (field "email" D.string)
        (field "bio" D.string)
        (field "image" D.string)
        (field "created" Iso8601.decoder)


profilesDecoder : Decoder (List Profile)
profilesDecoder =
    list profileDecoder

