module Api.Profile exposing (Profile, decoder)
import Json.Decode as D exposing (..)


type alias Profile =
    { id : Int
    , firstname : String
    , lastname : String
    , email : String
    , bio : String
    , image : String
    }

decoder : Decoder Profile
decoder =
    map6 Profile
        (field "id" D.int)
        (field "firstname" D.string)
        (field "lastname" D.string)
        (field "email" D.string)
        (field "bio" D.string)
        (field "image" D.string)

