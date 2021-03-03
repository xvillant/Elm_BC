module Components.Validity exposing (isValidEmail, isValidPassword)

import Regex exposing (Regex, contains, fromStringWith, never)


validEmail : Regex
validEmail =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


isValidEmail : String -> Bool
isValidEmail email =
    contains validEmail email


isValidPassword : String -> Bool
isValidPassword password =
    contains validPassword password


validPassword : Regex
validPassword =
    "^(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9])(?=.*?[#?!@$%^&*-]).{8,}$"
        |> fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never
