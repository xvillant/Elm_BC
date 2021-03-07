module Api.Token exposing (decodeJWT, Token)

import Json.Decode as D exposing (Decoder, field)
import Json.Decode.Extra exposing (andMap)
import Jwt


type alias Token =
    { iat : Int
    , exp : Int
    , userId : String
    , email : String
    }

decodeJWT : String -> Result Jwt.JwtError Token
decodeJWT tokenString =
    Jwt.decodeToken jwtDecoder tokenString


jwtDecoder : Decoder Token
jwtDecoder =
    D.succeed Token
        |> andMap (field "iat" D.int)
        |> andMap (field "exp" D.int)
        |> andMap (field "sub" D.string)
        |> andMap (field "email" D.string)