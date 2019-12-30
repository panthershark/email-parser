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
        , describe "isValid"
            [ test "valid email" <|
                \_ ->
                    isValid "hello@world.com"
                        |> Expect.equal True
            , test "valid email, multiple dots in domain parts" <|
                \_ ->
                    isValid "user@one.two.three"
                        |> Expect.equal True
            , test "invalid char in local part" <|
                \_ ->
                    isValid "he^llo@world.com"
                        |> Expect.equal False
            , test "invalid char in domain part" <|
                \_ ->
                    isValid "hello@worl^d.com"
                        |> Expect.equal False
            , test "bad domain" <|
                \_ ->
                    isValid "hello@world"
                        |> Expect.equal False
            , test "missing domain" <|
                \_ ->
                    isValid "hello@"
                        |> Expect.equal False
            , test "missing local" <|
                \_ ->
                    isValid "@world.com"
                        |> Expect.equal False
            ]
        , describe "toString"
            [ test "happy path" <|
                \_ ->
                    toString { local = "hello", domain = "world.com" }
                        |> Expect.equal "hello@world.com"
            ]
        ]
