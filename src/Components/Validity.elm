module Components.Validity exposing (isValidEmail, isValidPassword, containsUpperCase, containsDigit, containsLowerCase, containsChar)

import Regex exposing (Regex, contains, fromStringWith)


validEmail : Regex
validEmail =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


validPassword : Regex
validPassword =
    "^(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9])(?=.*?[-+_!@#$%^&*.,?]).{8,}$"
        |> fromStringWith { caseInsensitive = False, multiline = False }
        |> Maybe.withDefault Regex.never


isValidEmail : String -> Bool
isValidEmail email =
    contains validEmail email


isValidPassword : String -> Bool
isValidPassword password =
    contains validPassword password


upperCaseReg : Regex
upperCaseReg = "(?=.*[A-Z])" 
        |> fromStringWith { caseInsensitive = False, multiline = False }
        |> Maybe.withDefault Regex.never


lowerCaseReg : Regex
lowerCaseReg = "(?=.*[a-z])" 
        |> fromStringWith { caseInsensitive = False, multiline = False }
        |> Maybe.withDefault Regex.never

digitCaseReg : Regex
digitCaseReg = "(?=.*[0-9])" 
        |> fromStringWith { caseInsensitive = False, multiline = False }
        |> Maybe.withDefault Regex.never

specCharCaseReg : Regex
specCharCaseReg = "(?=.*[-+_!@#$%^&*.,?])" 
        |> fromStringWith { caseInsensitive = False, multiline = False }
        |> Maybe.withDefault Regex.never

containsLowerCase : String -> Bool
containsLowerCase password =
    contains lowerCaseReg password

containsUpperCase : String -> Bool
containsUpperCase password =
    contains upperCaseReg password

containsDigit : String -> Bool
containsDigit password =
    contains digitCaseReg password

containsChar : String -> Bool
containsChar password =
    contains specCharCaseReg password