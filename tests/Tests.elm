module Tests exposing (..)

import Test exposing (Test)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Bidirectional as Json
import Json.Decode as Decode
import Json.Encode as Encode
import Dict exposing (Dict)


testFuzzedCoderRoundTrip : String -> Fuzzer a -> Fuzzer (Json.Coder a) -> Test
testFuzzedCoderRoundTrip description valueFuzzer coderFuzzer =
    Test.fuzz (Fuzz.map2 (,) valueFuzzer coderFuzzer) description <|
        \( value, coder ) ->
            value
                |> Json.encodeValue coder
                |> Json.decodeValue coder
                |> Expect.equal (Ok value)


testCoderRoundTrip : String -> Fuzzer a -> Json.Coder a -> Test
testCoderRoundTrip description fuzzer coder =
    testFuzzedCoderRoundTrip description fuzzer (Fuzz.constant coder)


type alias Thing =
    { a : String
    , b : Bool
    , c : Int
    }


thingCoder : Json.Coder Thing
thingCoder =
    Json.object Thing
        |> Json.withField "a" .a Json.string
        |> Json.withField "b" .b Json.bool
        |> Json.withField "c" .c Json.int


type BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) (BinaryTree a)


binaryTreeFuzzer : Fuzz.Fuzzer a -> Fuzz.Fuzzer (BinaryTree a)
binaryTreeFuzzer valueFuzzer =
    let
        go maxDepth =
            if maxDepth <= 0 then
                Fuzz.map Leaf valueFuzzer
            else
                let
                    recurse =
                        go (maxDepth - 1)
                in
                    Fuzz.frequency
                        [ ( 1, Fuzz.map Leaf valueFuzzer )
                        , ( 2, Fuzz.map2 Branch recurse recurse )
                        ]
    in
        go 2


leafCoder : Json.Coder a -> Json.Coder a
leafCoder valueCoder =
    Json.at [ "leaf" ] valueCoder


branchCoder : Json.Coder a -> Json.Coder ( BinaryTree a, BinaryTree a )
branchCoder valueCoder =
    let
        node =
            Json.lazy (\_ -> binaryTreeCoder valueCoder)
    in
        Json.at [ "branch" ] (Json.tuple ( node, node ))


binaryTreeencodeValue : Json.Coder a -> BinaryTree a -> Json.Value
binaryTreeencodeValue valueCoder node =
    case node of
        Leaf value ->
            Json.encodeValue (leafCoder valueCoder) value

        Branch left right ->
            Json.encodeValue (branchCoder valueCoder) ( left, right )


binaryTreeDecoder : Json.Coder a -> Decode.Decoder (BinaryTree a)
binaryTreeDecoder valueCoder =
    Decode.oneOf
        [ Decode.map Leaf (Json.decoder (leafCoder valueCoder))
        , Decode.map (uncurry Branch) (Json.decoder (branchCoder valueCoder))
        ]


binaryTreeCoder : Json.Coder a -> Json.Coder (BinaryTree a)
binaryTreeCoder valueCoder =
    Json.custom
        (binaryTreeencodeValue valueCoder)
        (binaryTreeDecoder valueCoder)


dictFuzzer : Fuzzer a -> Fuzzer (Dict String a)
dictFuzzer valueFuzzer =
    Fuzz.map2 (,) Fuzz.string valueFuzzer
        |> Fuzz.list
        |> Fuzz.map Dict.fromList


resultCoder : Json.Coder err -> Json.Coder ok -> Json.Coder (Result err ok)
resultCoder errCoder okCoder =
    let
        wrappedErr =
            Json.at [ "err" ] errCoder

        wrappedOk =
            Json.at [ "ok" ] okCoder
    in
        Json.custom
            (\result ->
                case result of
                    Err x ->
                        Json.encodeValue wrappedErr x

                    Ok x ->
                        Json.encodeValue wrappedOk x
            )
            (Decode.oneOf
                [ Decode.map Err (Json.decoder wrappedErr)
                , Decode.map Ok (Json.decoder wrappedOk)
                ]
            )


type Constant a
    = Constant a


all : Test
all =
    Test.describe "Json.Bidirectional"
        [ Test.describe "Fuzz tests"
            [ testCoderRoundTrip "string"
                Fuzz.string
                Json.string
            , testCoderRoundTrip "bool"
                Fuzz.bool
                Json.bool
            , testCoderRoundTrip "int"
                Fuzz.int
                Json.int
            , testCoderRoundTrip "float"
                Fuzz.float
                Json.float
            , testCoderRoundTrip "nullable"
                (Fuzz.maybe Fuzz.int)
                (Json.nullable Json.int)
            , testCoderRoundTrip "list"
                (Fuzz.list Fuzz.int)
                (Json.list Json.int)
            , testCoderRoundTrip "object ... |> withField ... |> withField ..."
                (Fuzz.map3 Thing Fuzz.string Fuzz.bool Fuzz.int)
                thingCoder
            , testFuzzedCoderRoundTrip "at"
                Fuzz.int
                (Fuzz.list Fuzz.string
                    |> Fuzz.map (\keyPath -> Json.at keyPath Json.int)
                )
            , testCoderRoundTrip "dict"
                (dictFuzzer Fuzz.int)
                (Json.dict Json.int)
            , Test.fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.string, Fuzz.int ))) "keyValuePairs" <|
                \pairs ->
                    let
                        sortedDedupedPairs =
                            pairs
                                |> Dict.fromList
                                |> Dict.toList
                                |> List.sort

                        coder =
                            Json.keyValuePairs Json.int
                    in
                        sortedDedupedPairs
                            |> Json.encodeValue coder
                            |> Json.decodeValue coder
                            |> Result.map List.sort
                            |> Expect.equal (Ok sortedDedupedPairs)
            , Test.fuzz3 Fuzz.string Fuzz.int Fuzz.int "keyValuePairs repeated key takes last value" <|
                \k v1 v2 ->
                    let
                        coder =
                            Json.keyValuePairs Json.int
                    in
                        [ ( k, v1 ), ( k, v2 ) ]
                            |> Json.encodeValue coder
                            |> Json.decodeValue coder
                            |> Expect.equal (Ok [ ( k, v2 ) ])
            , testCoderRoundTrip "tuple"
                (Fuzz.tuple ( Fuzz.string, Fuzz.bool ))
                (Json.tuple ( Json.string, Json.bool ))
            , testCoderRoundTrip "tuple3"
                (Fuzz.tuple3 ( Fuzz.string, Fuzz.bool, Fuzz.int ))
                (Json.tuple3 ( Json.string, Json.bool, Json.int ))
            , testCoderRoundTrip "tuple4"
                (Fuzz.tuple4 ( Fuzz.string, Fuzz.bool, Fuzz.int, Fuzz.float ))
                (Json.tuple4 ( Json.string, Json.bool, Json.int, Json.float ))
            , testCoderRoundTrip "tuple5"
                (Fuzz.tuple5
                    ( Fuzz.string
                    , Fuzz.bool
                    , Fuzz.int
                    , Fuzz.float
                    , (dictFuzzer Fuzz.int)
                    )
                )
                (Json.tuple5
                    ( Json.string
                    , Json.bool
                    , Json.int
                    , Json.float
                    , (Json.dict Json.int)
                    )
                )
            , testCoderRoundTrip "BinaryTree coder using custom and lazy"
                (binaryTreeFuzzer Fuzz.int)
                (binaryTreeCoder Json.int)
            , testCoderRoundTrip "value"
                (Fuzz.map Encode.string Fuzz.string)
                Json.value
            , testCoderRoundTrip "resultCoder"
                (Fuzz.result Fuzz.string Fuzz.int)
                (resultCoder Json.string Json.int)
            , testCoderRoundTrip "bimap"
                (Fuzz.map Constant Fuzz.int)
                (Json.bimap (\(Constant x) -> x) Constant Json.int)
            ]
        ]
