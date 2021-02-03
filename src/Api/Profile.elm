module Api.Profile exposing (..)
import Json.Decode as D

type alias Profile =
    { username : String
    , bio : Maybe String
    }


decoder : D.Decoder Profile
decoder =
    D.map2 Profile
        (D.field "username" D.string)
        (D.field "bio" (D.maybe D.string))