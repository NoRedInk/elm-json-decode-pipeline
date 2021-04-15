module Tests exposing (all, expectErr, isError, runWith)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (Decoder, null, string)
import Json.Decode.Pipeline
    exposing
        ( optional
        , optionalAt
        , required
        , requiredAt
        , resolve
        )
import Json.Encode as Encode
import Test exposing (..)


{-| Run some JSON through a Decoder and return the result.
-}
runWith : String -> Decoder a -> Result Decode.Error a
runWith str decoder =
    Decode.decodeString decoder str


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
        , test "optional preserves Error structure if field is present but doesn't decode" <|
            \() ->
                let
                    expectedError =
                        Decode.Field "b" <|
                            Decode.OneOf
                                [ Decode.Failure "Expecting a STRING" (Encode.int 42)
                                , Decode.Failure "Expecting null" (Encode.int 42)
                                ]
                in
                Decode.succeed Tuple.pair
                    |> optional "a" string "--"
                    |> optional "b" string "--"
                    |> runWith """{"a":{},"b":42}"""
                    |> Expect.equal (Err expectedError)
        , test "optionalAt preserves Error structure if field is present but doesn't decode" <|
            \() ->
                let
                    expectedError =
                        Decode.Field "a" <|
                            Decode.Field "c" <|
                                Decode.OneOf
                                    [ Decode.Failure "Expecting a STRING" (Encode.int 42)
                                    , Decode.Failure "Expecting null" (Encode.int 42)
                                    ]
                in
                Decode.succeed Tuple.pair
                    |> optionalAt [ "a", "b" ] string "--"
                    |> optionalAt [ "a", "c" ] string "--"
                    |> runWith """{"a":{"b":{}, "c":42}}"""
                    |> Expect.equal (Err expectedError)
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
        ]
