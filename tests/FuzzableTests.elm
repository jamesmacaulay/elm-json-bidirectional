module FuzzableTests exposing (..)

import Test exposing (Test)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Bidirectional.Fuzzable as Json
import Json.Decode as Decode
import Dict exposing (Dict)


expectFuzzedCoderRoundTrip :
    (Result String a -> Result String a -> Expect.Expectation)
    -> String
    -> Fuzzer (Json.Coder a)
    -> Test
expectFuzzedCoderRoundTrip expectation description coderFuzzer =
    let
        valueCoderTupleFuzzer =
            coderFuzzer
                |> Fuzz.andThen
                    (\coder ->
                        Json.fuzzer coder
                            |> Fuzz.map (\value -> ( value, coder ))
                    )
    in
        Test.fuzz valueCoderTupleFuzzer description <|
            \( value, coder ) ->
                value
                    |> Json.encodeValue coder
                    |> Json.decodeValue coder
                    |> expectation (Ok value)


testFuzzedCoderRoundTrip : String -> Fuzzer (Json.Coder a) -> Test
testFuzzedCoderRoundTrip =
    expectFuzzedCoderRoundTrip Expect.equal


testCoderRoundTrip : String -> Json.Coder a -> Test
testCoderRoundTrip description coder =
    testFuzzedCoderRoundTrip description (Fuzz.constant coder)


testBadCoderRoundTripCaughtByFuzzer : String -> Json.Coder a -> Test
testBadCoderRoundTripCaughtByFuzzer description coder =
    expectFuzzedCoderRoundTrip Expect.notEqual description (Fuzz.constant coder)


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
        (binaryTreeFuzzer (Json.fuzzer valueCoder))


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
            (Fuzz.result
                (Json.fuzzer errCoder)
                (Json.fuzzer okCoder)
            )


type Constant a
    = Constant a


type alias EmailContact =
    { name : String
    , email : String
    }


type StringPair
    = StringPair String String


all : Test
all =
    Test.describe "Json.Bidirectional"
        [ Test.describe "Fuzz tests"
            [ testCoderRoundTrip "string"
                Json.string
            , testCoderRoundTrip "bool"
                Json.bool
            , testCoderRoundTrip "int"
                Json.int
            , testCoderRoundTrip "float"
                Json.float
            , testCoderRoundTrip "nullable"
                (Json.nullable Json.int)
            , testCoderRoundTrip "list"
                (Json.list Json.int)
            , testCoderRoundTrip "object ... |> withField ... |> withField ..."
                thingCoder
            , testFuzzedCoderRoundTrip "at"
                (Fuzz.list Fuzz.string
                    |> Fuzz.map (\keyPath -> Json.at keyPath Json.int)
                )
            , testCoderRoundTrip "dict"
                (Json.dict Json.int)
            , testCoderRoundTrip "tuple"
                (Json.tuple ( Json.string, Json.bool ))
            , testCoderRoundTrip "tuple3"
                (Json.tuple3 ( Json.string, Json.bool, Json.int ))
            , testCoderRoundTrip "tuple4"
                (Json.tuple4 ( Json.string, Json.bool, Json.int, Json.float ))
            , testCoderRoundTrip "tuple5"
                (Json.tuple5
                    ( Json.string
                    , Json.bool
                    , Json.int
                    , Json.float
                    , (Json.dict Json.int)
                    )
                )
            , testCoderRoundTrip "BinaryTree coder using custom and lazy"
                (binaryTreeCoder Json.int)
            , testCoderRoundTrip "value"
                Json.value
            , testCoderRoundTrip "resultCoder"
                (resultCoder Json.string Json.int)
            , testCoderRoundTrip "bimap"
                (Json.bimap (\(Constant x) -> x) Constant Json.int)
            , testBadCoderRoundTripCaughtByFuzzer "object with fields in the wrong order"
                (Json.object EmailContact
                    |> Json.withField "email" .email Json.string
                    |> Json.withField "name" .name Json.string
                )
            , testBadCoderRoundTripCaughtByFuzzer "bimap with functions that don't mirror each other"
                (Json.tuple ( Json.string, Json.string )
                    |> Json.bimap
                        (\(StringPair left right) -> ( left, left ))
                        (\( left, right ) -> StringPair left right)
                )
            ]
        ]
