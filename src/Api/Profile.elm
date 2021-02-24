module Api.Profile exposing (Profile, profileDecoder)
import Json.Decode as D exposing (..)


type alias Profile =
    { id : Int
    , firstname : String
    , lastname : String
    , email : String
    , bio : String
    , image : String
    }

profileDecoder : Decoder Profile
profileDecoder =
    map6 Profile
        (field "id" D.int)
        (field "firstname" D.string)
        (field "lastname" D.string)
        (field "email" D.string)
        (field "bio" D.string)
        (field "image" D.string)

