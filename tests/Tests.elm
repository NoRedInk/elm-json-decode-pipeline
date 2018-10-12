module Tests exposing (all, expectErr, isError, runWith)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (Decoder, null, string)
import Json.Decode.Pipeline
    exposing
        ( optional
        , optionalAt
        , required
        , requiredAt
        , ignoreRequired
        , resolve
        )
import Test exposing (..)


{-| Run some JSON through a Decoder and return the result.
-}
runWith : String -> Decoder a -> Result String a
runWith str decoder =
    Decode.decodeString decoder str
        |> Result.mapError Decode.errorToString


isError : Result err ok -> Bool
isError result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False


expectErr : Result err ok -> Expectation
expectErr result =
    isError result
        |> Expect.true ("Expected an Err but got " ++ Debug.toString result)


all : Test
all =
    describe
        "Json.Decode.Pipeline"
        [ test "should decode basic example" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> required "a" string
                    |> required "b" string
                    |> runWith """{"a":"foo","b":"bar"}"""
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "should decode requiredAt fields" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> requiredAt [ "a" ] string
                    |> requiredAt [ "b", "c" ] string
                    |> runWith """{"a":"foo","b":{"c":"bar"}}"""
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "should decode optionalAt fields" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> optionalAt [ "a", "b" ] string "--"
                    |> optionalAt [ "x", "y" ] string "--"
                    |> runWith """{"a":{},"x":{"y":"bar"}}"""
                    |> Expect.equal (Ok ( "--", "bar" ))
        , test "optional succeeds if the field is not present" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """{"x":"five"}"""
                    |> Expect.equal (Ok ( "--", "five" ))
        , test "optional succeeds with fallback if the field is present but null" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal (Ok ( "--", "five" ))
        , test "optional succeeds with result of the given decoder if the field is null and the decoder decodes nulls" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> optional "a" (null "null") "--"
                    |> optional "x" string "--"
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal (Ok ( "null", "five" ))
        , test "optional fails if the field is present but doesn't decode" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """{"x":5}"""
                    |> expectErr
        , test "optionalAt fails if the field is present but doesn't decode" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> optionalAt [ "a", "b" ] string "--"
                    |> optionalAt [ "x", "y" ] string "--"
                    |> runWith """{"a":{},"x":{"y":5}}"""
                    |> expectErr
        , test "resolve bubbles up decoded Err results" <|
            \() ->
                Decode.succeed Decode.fail
                    |> required "error" string
                    |> resolve
                    |> runWith """{"error":"invalid"}"""
                    |> expectErr
        , test "resolve bubbles up decoded Ok results" <|
            \() ->
                Decode.succeed Decode.succeed
                    |> required "ok" string
                    |> resolve
                    |> runWith """{"ok":"valid"}"""
                    |> Expect.equal (Ok "valid")
        , test "ignoreRequired fails if the field is not present" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> required "firstname" string
                    |> required "lastname" string
                    |> ignoreRequired "missing" (Decode.succeed ())
                    |> runWith """{"firstname": "john", "lastname": "doe"}"""
                    |> expectErr
        , test "ignoreRequired fails if the field is present and the value decoder fails" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> required "firstname" string
                    |> required "lastname" string
                    |> ignoreRequired "missing" (Decode.fail "always failing")
                    |> runWith """{"firstname": "john", "lastname": "doe", "missing": "field"}"""
                    |> expectErr
        , test "ignoreRequired succeeds if the field is present and the value decoder doesn't fail" <|
            \() ->
                Decode.succeed Tuple.pair
                    |> required "firstname" string
                    |> required "lastname" string
                    |> ignoreRequired "missing" (Decode.succeed ())
                    |> runWith """{"firstname": "john", "lastname": "doe", "missing": "field"}"""
                    |> Expect.equal (Ok ("john","doe"))
        ]
