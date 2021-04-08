module Api.Token exposing (decodeJWT, Token)

import Json.Decode as D exposing (Decoder, field, map4)
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
    map4 Token
        (field "iat" D.int)
        (field "exp" D.int)
        (field "sub" D.string)
        (field "email" D.string)