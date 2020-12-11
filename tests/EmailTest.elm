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
        , describe "valid emails" <| validEmailTests
        , describe "invalid emails" <| invalidEmailTests
        , describe "toString"
            [ test "happy path" <|
                \_ ->
                    toString { local = "hello", domain = "world.com" }
                        |> Expect.equal "hello@world.com"
            ]
        ]



-- Data


type alias ValidEmailTest =
    { description : String
    , email : EmailAddress
    }


validEmails : List ValidEmailTest
validEmails =
    [ { description = "valid email"
      , email = { local = "hello", domain = "world.com" }
      }
    , { description = "valid email - dot in local part"
      , email = { local = "hello.world", domain = "world.com" }
      }
    , { description = "valid email - multiple dots in local part"
      , email = { local = "h.e.l.l.o", domain = "world.com" }
      }
    , { description = "valid email, multiple dots in domain parts"
      , email = { local = "user", domain = "one.two.three" }
      }
    , { description = "+ sign in local part"
      , email = { local = "firstname+lastname", domain = "domain.com" }
      }
    , { description = "dash sign in local part"
      , email = { local = "firstname-lastname", domain = "domain.com" }
      }
    , { description = "dash in domain part"
      , email = { local = "email", domain = "domain-one.com" }
      }
    , { description = "multiple dots in domain part"
      , email = { local = "firstname", domain = "example.co.uk" }
      }
    , { description = "ipv4 address for domain"
      , email = { local = "email", domain = "123.123.123.123" }
      }
    ]


type alias InvalidEmailTest =
    { description : String
    , email : String
    }


invalidEmails : List InvalidEmailTest
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
    , { description = "domain part ends with a dot"
      , email = "email@domain."
      }
    ]



-- Helpers


toValidTest : ValidEmailTest -> Test
toValidTest { description, email } =
    test description <|
        \_ -> parse (email.local ++ "@" ++ email.domain) |> Expect.equal (Ok email)


toInvalidTest : InvalidEmailTest -> Test
toInvalidTest { description, email } =
    test description <|
        \_ -> isValid email |> Expect.equal False


validEmailTests : List Test
validEmailTests =
    List.map toValidTest validEmails


invalidEmailTests : List Test
invalidEmailTests =
    List.map toInvalidTest invalidEmails
