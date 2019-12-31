module EmailTest exposing (suite)

import Email exposing (EmailAddress, isValid, parse, toString)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Email"
        [ describe "parse"
            [ test "valid email" <|
                \_ ->
                    parse "hello@world.com"
                        |> Expect.equal (Ok { local = "hello", domain = "world.com" })
            , test "invalid email" <|
                \_ ->
                    parse "helloworld.com"
                        |> Expect.err
            ]
        , describe "isValid" <| validEmailTests ++ invalidEmailTests
        , describe "toString"
            [ test "happy path" <|
                \_ ->
                    toString { local = "hello", domain = "world.com" }
                        |> Expect.equal "hello@world.com"
            ]
        ]



-- Data


type alias EmailTest =
    { description : String
    , email : String
    }


validEmails : List EmailTest
validEmails =
    [ { description = "valid email"
      , email = "hello@world.com"
      }
    , { description = "valid email - dot in local part"
      , email = "hello.world@world.com"
      }
    , { description = "valid email - multiple dots in local part"
      , email = "h.e.l.l.o@world.com"
      }
    , { description = "valid email, multiple dots in domain parts"
      , email = "user@one.two.three"
      }
    , { description = "+ sign in local part"
      , email = "firstname+lastname@domain.com"
      }
    , { description = "dash sign in local part"
      , email = "firstname-lastname@domain.com"
      }
    , { description = "dash in domain part"
      , email = "email@domain-one.com"
      }
    , { description = "multiple dots in domain part"
      , email = "firstname@example.co.uk"
      }
    ]


invalidEmails : List EmailTest
invalidEmails =
    [ { description = "invalid char in local part"
      , email = "he^llo@world.com"
      }
    , { description = "invalid char in domain part"
      , email = "hello@worl^d.com"
      }
    , { description = "bad domain"
      , email = "hello@world"
      }
    , { description = "missing domain"
      , email = "hello@"
      }
    , { description = "missing local"
      , email = "@world.com"
      }
    , { description = "local part is whitespace - not quoted"
      , email = " @example.org"
      }
    , { description = "local part has 2 consecutive dots"
      , email = "john..doe@example.org"
      }
    , { description = "local part starts with dot"
      , email = ".john@example.org"
      }
    , { description = "local part ends with dot"
      , email = "john.@example.org"
      }
    , { description = "no @ sign"
      , email = "example.com"
      }
    , { description = "two @ signs"
      , email = "email@domain@domain.com"
      }
    , { description = "unquoted leading spaces"
      , email = "my-name email@domain.com"
      }
    , { description = "unquoted trailing spaces"
      , email = "email@domain.com my-name"
      }
    ]



-- Helpers


toIsValidTest : Bool -> EmailTest -> Test
toIsValidTest expect { description, email } =
    test description <|
        \_ -> isValid email |> Expect.equal expect


validEmailTests : List Test
validEmailTests =
    List.map (toIsValidTest True) validEmails


invalidEmailTests : List Test
invalidEmailTests =
    List.map (toIsValidTest False) invalidEmails
