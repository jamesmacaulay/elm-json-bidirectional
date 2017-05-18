module Json.Bidirectional.Fuzzable
    exposing
        ( Value
        , CoderBuilder
        , Coder
        , string
        , bool
        , int
        , float
        , nullable
        , list
        , object
        , withField
        , at
        , dict
        , tuple
        , tuple3
        , tuple4
        , tuple5
        , value
        , bimap
        , lazy
        , custom
        , encodeValue
        , encodeString
        , decodeValue
        , decodeString
        , decoder
        , fuzzer
        )

{-| The functions in this module let you build up two-way `Coder` structures that concisely specify how values of some Elm type can be both encoded to and decoded from JSON.

# Types

@docs Value, CoderBuilder, Coder

# Primitives

@docs string, bool, int, float

# Data Structures

@docs nullable, list, object, withField, at, dict, tuple, tuple3, tuple4, tuple5, value

# Fancy Stuff

@docs bimap, lazy, custom

# Encoding and Decoding

@docs encodeValue, encodeString, decodeValue, decodeString, decoder
-}

import Fuzz exposing (Fuzzer)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Bidirectional.Encoder as Encoder exposing (Encoder)
import Dict exposing (Dict)
import Tuple


{-| An alias for `Json.Encode.Value`. Can be used anywhere `Json.Encode.Value` or `Json.Decode.Value` is expected.
-}
type alias Value =
    Encode.Value


{-| This type is used to represent Coders that are still being built, which means that the type they encode from may be different than the type they decode to.
-}
type CoderBuilder a b
    = CoderTriple (Encoder a) (Decoder b) (Fuzzer b)


{-| An alias for `CoderBuilder a a`, this represents a symmetric pair of encoder and decoder.
-}
type alias Coder a =
    CoderBuilder a a


{-| A Coder for String values in Elm, encoded as JSON strings.
-}
string : Coder String
string =
    CoderTriple
        (Encoder.opaque Encode.string)
        Decode.string
        Fuzz.string


{-| A Coder for Bool values in Elm, encoded as JSON true or false.
-}
bool : Coder Bool
bool =
    CoderTriple
        (Encoder.opaque Encode.bool)
        Decode.bool
        Fuzz.bool


{-| A Coder for Int values in Elm, encoded as JSON numbers.
-}
int : Coder Int
int =
    CoderTriple
        (Encoder.opaque Encode.int)
        Decode.int
        Fuzz.int


{-| A Coder for Float values in Elm, encoded as JSON numbers.
-}
float : Coder Float
float =
    CoderTriple
        (Encoder.opaque Encode.float)
        Decode.float
        Fuzz.float


{-| Construct a Coder for Maybe values in Elm, where Nothing is encoded as null and Just values are encoded using the given Coder.
-}
nullable : Coder a -> Coder (Maybe a)
nullable (CoderTriple encoder decoder fuzzer) =
    CoderTriple
        (Encoder.nullable encoder)
        (Decode.nullable decoder)
        (Fuzz.maybe fuzzer)


{-| Construct a Coder for List values in Elm, encoded as JSON arrays whose items are encoded as per the given Coder.
-}
list : Coder a -> Coder (List a)
list (CoderTriple encoder decoder fuzzer) =
    CoderTriple
        (Encoder.opaque (List.map (Encoder.encodeValue encoder) >> Encode.list))
        (Decode.list decoder)
        (Fuzz.list fuzzer)


{-| Begin construction of a Coder for values of an arbitrary Elm type, encoded as JSON objects with specific fields, each encoded in their own way:

    type alias User =
        { name : String
        , isAdmin : Bool
        }

    userCoder : Coder User
    userCoder =
        object User
            |> withField "name" .name string
            |> withField "isAdmin" .isAdmin bool

The first argument is a constructor function that must take the values of the specified fields in order. Once all fields have been correctly specified by chaining `withField` calls in a pipeline, the types of the `CoderBuilder` converge and the result is a symmetric `Coder`.
-}
object : (a -> b) -> CoderBuilder c (a -> b)
object constructor =
    CoderTriple
        (Encoder.object [])
        (Decode.succeed constructor)
        (Fuzz.constant constructor)


{-| Adds a field to an object CoderBuilder pipeline. See `object` for an example.

The first argument is the name of the field as it appears in the JSON. The second argument is a getter function that is used in the encoding process that takes a value of the source type and returns a value for the field. The third argument is a Coder for the field value. The fourth argument is the object CoderBuilder (usually applied via the `|>` operator) whose constructor function expects the field value as its next argument. Once all of the constructor function's arguments have been correctly applied, the types will converge and the result is a symmetric Coder.
-}
withField :
    String
    -> (a -> b)
    -> Coder b
    -> CoderBuilder a (b -> c)
    -> CoderBuilder a c
withField name getter (CoderTriple valueEncoder valueDecoder valueFuzzer) (CoderTriple objectEncoder objectDecoder objectFuzzer) =
    let
        encoder =
            Encoder.withField name getter valueEncoder objectEncoder

        decoder =
            Decode.map2 (<|) objectDecoder (Decode.field name valueDecoder)

        fuzzer =
            Fuzz.map2 (<|) objectFuzzer valueFuzzer
    in
        CoderTriple encoder decoder fuzzer


{-| Transform a Coder such that its JSON representation is nested under the given list of object field names, from outermost to innermost. For example,

    fooBarIntCoder : Coder Int
    fooBarIntCoder =
        at [ "foo", "bar" ] int

Encoding the Elm Int `33` with `fooBarIntCoder` produces the following JSON value:

    {"foo": {"bar": 33}}

When decoding using the same Coder, the enclosed value is extracted from the specified nested fields.
-}
at : List String -> Coder a -> Coder a
at keyPath (CoderTriple encoder decoder fuzzer) =
    CoderTriple
        (Encoder.at keyPath encoder)
        (Decode.at keyPath decoder)
        fuzzer


foldPairIntoDictWithUniqueKeys : ( String, a ) -> Dict String a -> Fuzzer (Dict String a)
foldPairIntoDictWithUniqueKeys ( k, v ) dict =
    if Dict.member k dict then
        Fuzz.char
            |> Fuzz.andThen
                (\char ->
                    foldPairIntoDictWithUniqueKeys ( String.cons char k, v ) dict
                )
    else
        Fuzz.constant (Dict.insert k v dict)


dictFuzzer : Fuzzer a -> Fuzzer (Dict String a)
dictFuzzer valueFuzzer =
    Fuzz.map2 (,) Fuzz.string valueFuzzer
        |> Fuzz.list
        |> Fuzz.map Dict.fromList



-- |> Fuzz.andThen (List.foldl (Fuzz.andThen << foldPairIntoDictWithUniqueKeys) (Fuzz.constant Dict.empty))


{-| Construct a Coder for a Dict whose keys are Strings and whose values have the type of the given Coder argument. The Dict is encoded as a JSON object with an arbitrary list of fields, and each value encoded in the same way:

    dictJson : Value
    dictJson =
        [ ("foo", 1)
        , ("bar", 2)
        , ("baz", 3)
        ]
            |> Dict.fromList
            |> encodeValue (dict int)

With the above code, `dictJson` has the following structure:

    {"foo": 1, "bar": 2, "baz": 3}

Decoding is the same as with `Json.Decode.dict`.
-}
dict : Coder a -> Coder (Dict String a)
dict (CoderTriple encoder decoder fuzzer) =
    CoderTriple
        (Encoder.opaque
            (Dict.toList
                >> List.map (Tuple.mapSecond (Encoder.encodeValue encoder))
                >> Encode.object
            )
        )
        (Decode.dict decoder)
        (dictFuzzer fuzzer)


{-| Take a 2-tuple of Coders and produce a Coder of 2-tuples, encoding them as 2-element JSON arrays:

    intBoolTupleCoder : Coder ( Int, Bool )
    intBoolTupleCoder =
        tuple (int, bool)

    tupleJson : Value
    tupleJson =
        ( 109, True )
            |> encodeValue intBoolTupleCoder

With the above code, `tupleJson` is the JSON array `[109, true]`. Decoding uses the specified value Coder for each index of the JSON array.
-}
tuple : ( Coder a, Coder b ) -> Coder ( a, b )
tuple ( CoderTriple encA decA fuzzA, CoderTriple encB decB fuzzB ) =
    CoderTriple
        (Encoder.opaque
            (\( a, b ) ->
                Encode.list
                    [ Encoder.encodeValue encA a
                    , Encoder.encodeValue encB b
                    ]
            )
        )
        (Decode.map2 (,)
            (Decode.index 0 decA)
            (Decode.index 1 decB)
        )
        (Fuzz.map2 (,)
            fuzzA
            fuzzB
        )


{-| Take a 3-tuple of Coders and produce a Coder of 3-tuples, encoding them as 3-element JSON arrays. See `tuple` for more details.
-}
tuple3 : ( Coder a, Coder b, Coder c ) -> Coder ( a, b, c )
tuple3 ( CoderTriple encA decA fuzzA, CoderTriple encB decB fuzzB, CoderTriple encC decC fuzzC ) =
    CoderTriple
        (Encoder.opaque
            (\( a, b, c ) ->
                Encode.list
                    [ Encoder.encodeValue encA a
                    , Encoder.encodeValue encB b
                    , Encoder.encodeValue encC c
                    ]
            )
        )
        (Decode.map3 (,,)
            (Decode.index 0 decA)
            (Decode.index 1 decB)
            (Decode.index 2 decC)
        )
        (Fuzz.map3 (,,)
            fuzzA
            fuzzB
            fuzzC
        )


{-| Take a 4-tuple of Coders and produce a Coder of 4-tuples, encoding them as 4-element JSON arrays. See `tuple` for more details.
-}
tuple4 : ( Coder a, Coder b, Coder c, Coder d ) -> Coder ( a, b, c, d )
tuple4 ( CoderTriple encA decA fuzzA, CoderTriple encB decB fuzzB, CoderTriple encC decC fuzzC, CoderTriple encD decD fuzzD ) =
    CoderTriple
        (Encoder.opaque
            (\( a, b, c, d ) ->
                Encode.list
                    [ Encoder.encodeValue encA a
                    , Encoder.encodeValue encB b
                    , Encoder.encodeValue encC c
                    , Encoder.encodeValue encD d
                    ]
            )
        )
        (Decode.map4 (,,,)
            (Decode.index 0 decA)
            (Decode.index 1 decB)
            (Decode.index 2 decC)
            (Decode.index 3 decD)
        )
        (Fuzz.map4 (,,,)
            fuzzA
            fuzzB
            fuzzC
            fuzzD
        )


{-| Take a 5-tuple of Coders and produce a Coder of 5-tuples, encoding them as 5-element JSON arrays. See `tuple` for more details.
-}
tuple5 : ( Coder a, Coder b, Coder c, Coder d, Coder e ) -> Coder ( a, b, c, d, e )
tuple5 ( CoderTriple encA decA fuzzA, CoderTriple encB decB fuzzB, CoderTriple encC decC fuzzC, CoderTriple encD decD fuzzD, CoderTriple encE decE fuzzE ) =
    CoderTriple
        (Encoder.opaque
            (\( a, b, c, d, e ) ->
                Encode.list
                    [ Encoder.encodeValue encA a
                    , Encoder.encodeValue encB b
                    , Encoder.encodeValue encC c
                    , Encoder.encodeValue encD d
                    , Encoder.encodeValue encE e
                    ]
            )
        )
        (Decode.map5 (,,,,)
            (Decode.index 0 decA)
            (Decode.index 1 decB)
            (Decode.index 2 decC)
            (Decode.index 3 decD)
            (Decode.index 4 decE)
        )
        (Fuzz.map5 (,,,,)
            fuzzA
            fuzzB
            fuzzC
            fuzzD
            fuzzE
        )


valueFuzzer : Fuzzer Value
valueFuzzer =
    let
        scalarFrequencies =
            [ ( 1, Fuzz.map Encode.string Fuzz.string )
            , ( 1, Fuzz.map Encode.bool Fuzz.bool )
            , ( 1, Fuzz.map Encode.int Fuzz.int )
            , ( 1, Fuzz.map Encode.float Fuzz.float )
            , ( 1, Fuzz.constant Encode.null )
            ]

        scalarFuzzer =
            Fuzz.frequency scalarFrequencies

        compositeFuzzer freq valueFuzzer =
            Fuzz.frequency
                (scalarFrequencies
                    ++ [ ( freq, Fuzz.map Encode.list (Fuzz.list valueFuzzer) )
                       , ( freq
                         , Fuzz.map
                            (Encode.object << Dict.toList)
                            (dictFuzzer valueFuzzer)
                         )
                       ]
                )
    in
        compositeFuzzer 10 (compositeFuzzer 0.05 scalarFuzzer)


{-| A Coder for arbitrary JSON values that are left untouched in both the encoding and decoding processes.
-}
value : Coder Value
value =
    CoderTriple (Encoder.opaque identity) Decode.value valueFuzzer


{-| Map both the encoding and decoding processes of a Coder, producing a new Coder of a new type. The first argument is a function that maps over the encoding process, taking Elm values of type `b` and producing `a` values that the given `Coder a` knows how to encode. The second argument is a function that maps over the decoding process, taking results from the given `Coder a` and transforming them into the ones that are produced by the resulting `Coder b`. For example:

    setCoder : Coder Set
    setCoder =
        list
            |> bimap Set.toList Set.fromList
-}
bimap : (b -> a) -> (a -> b) -> Coder a -> Coder b
bimap ba ab (CoderTriple encoder decoder fuzzer) =
    CoderTriple
        (Encoder.opaque (ba >> Encoder.encodeValue encoder))
        (Decode.map ab decoder)
        (Fuzz.map ab fuzzer)


{-| This function makes it possible to define Coders for recursive JSON structures. Use it like you use `Json.Decode.lazy`:

    type alias Comment =
        { message : String
        , responses : Responses
        }

    type Responses = Responses (List Comment)

    responses : Coder Responses
    responses =
        bimap
            (\(Responses comments) -> comments)
            Responses
            (list (lazy (\_ -> comment)))

    comment : Coder Comment
    comment =
        object Comment
            |> withField "message" .message string
            |> withField "responses" .responses responses
-}
lazy : (() -> Coder a) -> Coder a
lazy getCoder =
    CoderTriple
        (Encoder.opaque (\x -> encodeValue (getCoder ()) x))
        (Decode.andThen (getCoder >> decoder) (Decode.succeed ()))
        (Fuzz.andThen (getCoder >> fuzzer) (Fuzz.constant ()))


{-| Construct a custom Coder out of an encoding function and a Decoder for the same Elm type. This is useful for defining Coders of union types:


    resultCoder : Coder err -> Coder ok -> Coder (Result err ok)
    resultCoder errCoder okCoder =
        let
            wrappedErr =
                at [ "err" ] errCoder

            wrappedOk =
                at [ "ok" ] okCoder
        in
            custom
                (\result ->
                    case result of
                        Err x ->
                            encodeValue wrappedErr x

                        Ok x ->
                            encodeValue wrappedOk x
                )
                (Decode.oneOf
                    [ Decode.map Err (decoder wrappedErr)
                    , Decode.map Ok (decoder wrappedOk)
                    ]
                )
-}
custom : (a -> Value) -> Decoder a -> Fuzzer a -> Coder a
custom toValue decoder fuzzer =
    CoderTriple (Encoder.opaque toValue) decoder fuzzer


{-| Use a Coder to encode something to JSON as a Value, which can be used anywhere that `Json.Encode.Value` or `Json.Decode.Value` is expected.
-}
encodeValue : Coder a -> a -> Value
encodeValue (CoderTriple encoder _ _) =
    Encoder.encodeValue encoder


{-| Use a Coder to encode something to JSON as a String. The second argument is the amount of indentation to use for serialization, with `0` resulting in a one-line JSON String.
-}
encodeString : Coder a -> Int -> a -> String
encodeString coder indentation =
    encodeValue coder >> Encode.encode indentation


{-| Use a Coder to decode a JSON Value into the Coder's type. Returns a Result with String error messages because the structure of the JSON may not match what is expected by the Coder.
-}
decodeValue : Coder a -> Value -> Result String a
decodeValue (CoderTriple _ decoder _) =
    Decode.decodeValue decoder


{-| Use a Coder to decode a JSON String into the Coder's type. Returns a Result with String error messages because the structure of the JSON may not match what is expected by the Coder.
-}
decodeString : Coder a -> String -> Result String a
decodeString (CoderTriple _ decoder _) =
    Decode.decodeString decoder


{-| Get a `Json.Decode.Decoder` from a `Coder`.
-}
decoder : Coder a -> Decoder a
decoder (CoderTriple _ decoder _) =
    decoder


{-| Get a `Fuzz.Fuzzer` from a `Coder`.
-}
fuzzer : Coder a -> Fuzzer a
fuzzer (CoderTriple _ _ fuzzer) =
    fuzzer
