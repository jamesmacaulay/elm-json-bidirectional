module Json.Bidirectional
    exposing
        ( Value
        , CoderBuilder
        , Coder
        , string
        , int
        , float
        , bool
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
        , lazy
        , value
        , custom
        , encodeValue
        , encodeString
        , decodeValue
        , decodeString
        , decoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Bidirectional.Encoder as Encoder exposing (Encoder)
import Dict exposing (Dict)
import Tuple


type alias Value =
    Encode.Value


type CoderBuilder a b
    = CoderPair (Encoder a) (Decoder b)


type alias Coder a =
    CoderBuilder a a


string : Coder String
string =
    CoderPair
        (Encoder.opaque Encode.string)
        Decode.string


bool : Coder Bool
bool =
    CoderPair
        (Encoder.opaque Encode.bool)
        Decode.bool


int : Coder Int
int =
    CoderPair
        (Encoder.opaque Encode.int)
        Decode.int


float : Coder Float
float =
    CoderPair
        (Encoder.opaque Encode.float)
        Decode.float


nullable : Coder a -> Coder (Maybe a)
nullable (CoderPair encoder decoder) =
    CoderPair
        (Encoder.nullable encoder)
        (Decode.nullable decoder)


list : Coder a -> Coder (List a)
list (CoderPair encoder decoder) =
    CoderPair
        (Encoder.opaque (List.map (Encoder.encodeValue encoder) >> Encode.list))
        (Decode.list decoder)


object : a -> CoderBuilder b a
object constructor =
    CoderPair
        (Encoder.object [])
        (Decode.succeed constructor)


withField :
    String
    -> (a -> b)
    -> CoderBuilder b c
    -> CoderBuilder a (c -> d)
    -> CoderBuilder a d
withField name getter (CoderPair valueEncoder valueDecoder) (CoderPair objectEncoder objectDecoder) =
    let
        encoder =
            Encoder.withField name getter valueEncoder objectEncoder

        decoder =
            Decode.map2 (<|) objectDecoder (Decode.field name valueDecoder)
    in
        CoderPair encoder decoder


at : List String -> Coder a -> Coder a
at keyPath (CoderPair encoder decoder) =
    CoderPair
        (Encoder.at keyPath encoder)
        (Decode.at keyPath decoder)


dict : Coder a -> Coder (Dict String a)
dict (CoderPair encoder decoder) =
    CoderPair
        (Encoder.opaque
            (Dict.toList
                >> List.map (Tuple.mapSecond (Encoder.encodeValue encoder))
                >> Encode.object
            )
        )
        (Decode.dict decoder)


tuple : ( Coder a, Coder b ) -> Coder ( a, b )
tuple ( CoderPair encA decA, CoderPair encB decB ) =
    CoderPair
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


tuple3 : ( Coder a, Coder b, Coder c ) -> Coder ( a, b, c )
tuple3 ( CoderPair encA decA, CoderPair encB decB, CoderPair encC decC ) =
    CoderPair
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


tuple4 : ( Coder a, Coder b, Coder c, Coder d ) -> Coder ( a, b, c, d )
tuple4 ( CoderPair encA decA, CoderPair encB decB, CoderPair encC decC, CoderPair encD decD ) =
    CoderPair
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


tuple5 : ( Coder a, Coder b, Coder c, Coder d, Coder e ) -> Coder ( a, b, c, d, e )
tuple5 ( CoderPair encA decA, CoderPair encB decB, CoderPair encC decC, CoderPair encD decD, CoderPair encE decE ) =
    CoderPair
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


lazy : (() -> Coder a) -> Coder a
lazy getCoder =
    CoderPair
        (Encoder.opaque (\x -> encodeValue (getCoder ()) x))
        (Decode.andThen (getCoder >> decoder) (Decode.succeed ()))


value : Coder Value
value =
    CoderPair (Encoder.opaque identity) Decode.value


custom : (a -> Value) -> Decoder a -> Coder a
custom toValue decoder =
    CoderPair (Encoder.opaque toValue) decoder


encodeValue : Coder a -> a -> Value
encodeValue (CoderPair encoder _) =
    Encoder.encodeValue encoder


encodeString : Coder a -> Int -> a -> String
encodeString coder indentation =
    encodeValue coder >> Encode.encode indentation


decodeValue : Coder a -> Value -> Result String a
decodeValue (CoderPair _ decoder) =
    Decode.decodeValue decoder


decodeString : Coder a -> String -> Result String a
decodeString (CoderPair _ decoder) =
    Decode.decodeString decoder


decoder : Coder a -> Decoder a
decoder (CoderPair _ decoder) =
    decoder
