module Email exposing (EmailAddress, isValid, parse, toString)

{-| Email parser and validation library.

@docs EmailAddress, isValid, parse, toString

-}

import Parser exposing ((|.), (|=), DeadEnd, Parser, andThen, chompUntil, chompWhile, end, getChompedString, problem, run, succeed, symbol)


{-| A model for representing an email. This is exposed, but you'll probably only use it is using parseEmailAddress
-}
type alias EmailAddress =
    { local : String
    , domain : String
    }


{-| Converts the EmailAddress model to a string.
-}
toString : EmailAddress -> String
toString { local, domain } =
    local ++ "@" ++ domain


{-| Given a string, parses into an EmailAddress model.

        parse "hello@world.com" == Ok { local = "hello", domain = "world.com" }
        parse "^^^^" == Err [ dead ends ]

-}
parse : String -> Result (List DeadEnd) EmailAddress
parse s =
    run emailParser s


{-| Given a string, this returns true if the email is compatible with the spec.

        isValid "hello@world.com" == True
        isValid "^^^^" == False

-}
isValid : String -> Bool
isValid s =
    run emailParser s
        |> Result.map (always True)
        |> Result.withDefault False


checkLocal : String -> Parser String
checkLocal str =
    let
        isLocalChar c =
            Char.isAlphaNum c
                || (c == '!')
                || (c == '#')
                || (c == '$')
                || (c == '%')
                || (c == '&')
                || (c == '*')
                || (c == '_')
                || (c == '-')
                || (c == '~')
                || (c == '|')
                || (c == '+')
                || (c == '=')
                || (c == '`')
                || (c == '{')
                || (c == '}')
    in
    if String.isEmpty str then
        problem "local part of email is empty"

    else if String.foldl (\c acc -> acc && isLocalChar c) True str then
        succeed str

    else
        problem "local part is not valid"


localParser : Parser String
localParser =
    chompUntil "@"
        |> getChompedString
        |> andThen checkLocal


domainPart : Parser String
domainPart =
    let
        checkLen s =
            if String.isEmpty s then
                problem "domain is not valid"

            else
                succeed s
    in
    chompWhile
        (\c -> Char.isAlphaNum c || c == '-')
        |> getChompedString
        |> andThen checkLen


domainParser : Parser String
domainParser =
    succeed (\pre suf -> pre ++ "." ++ suf)
        |= domainPart
        |. symbol "."
        |= domainPart


emailParser : Parser EmailAddress
emailParser =
    succeed EmailAddress
        |= localParser
        |. symbol "@"
        |= domainParser
        |. end
