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
            , test "valid email - dot in local part" <|
                \_ ->
                    isValid "hello.world@world.com"
                        |> Expect.equal True
            , test "valid email - multiple dots in local part" <|
                \_ ->
                    isValid "h.e.l.l.o@world.com"
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
        , describe "obscure email variations (these are valid if quoted, but RFC 5321 warns against it and this is accepted as a bad idea)"
            [ test "local part is whitespace - not quoted" <|
                \_ ->
                    isValid " @example.org"
                        |> Expect.equal False
            , test "local part has 2 consecutive dots" <|
                \_ ->
                    isValid "john..doe@example.org"
                        |> Expect.equal False
            , test "local part starts with dot" <|
                \_ ->
                    isValid ".john@example.org"
                        |> Expect.equal False
            , test "local part ends with dot" <|
                \_ ->
                    isValid "john.@example.org"
                        |> Expect.equal False
            ]
        , describe "toString"
            [ test "happy path" <|
                \_ ->
                    toString { local = "hello", domain = "world.com" }
                        |> Expect.equal "hello@world.com"
            ]
        ]
