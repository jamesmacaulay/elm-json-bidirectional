module Json.Bidirectional.Encoder
    exposing
        ( Encoder
        , opaque
        , object
        , nullable
        , withField
        , encodeValue
        , contramap
        , at
        )

import Json.Encode as Encode exposing (Value)


type Encoder a
    = Opaque (a -> Value)
    | Object (List ( String, a -> Value ))


opaque : (a -> Value) -> Encoder a
opaque =
    Opaque


object : List ( String, a -> Value ) -> Encoder a
object =
    Object


maybeValue : Encoder a -> Maybe a -> Value
maybeValue encoder maybeX =
    case maybeX of
        Nothing ->
            Encode.null

        Just x ->
            encodeValue encoder x


nullable : Encoder a -> Encoder (Maybe a)
nullable encoder =
    opaque (maybeValue encoder)


withField : String -> (a -> b) -> Encoder b -> Encoder a -> Encoder a
withField name getter valueEncoder objectEncoder =
    case objectEncoder of
        Object toValuePairs ->
            Object (toValuePairs ++ [ ( name, getter >> encodeValue valueEncoder ) ])

        Opaque _ ->
            objectEncoder


encodeValue : Encoder a -> a -> Value
encodeValue encoder x =
    case encoder of
        Opaque toValue ->
            toValue x

        Object toValuePairs ->
            toValuePairs
                |> List.map (Tuple.mapSecond (\toValue -> toValue x))
                |> Encode.object


contramap : (a -> b) -> Encoder b -> Encoder a
contramap f encoder =
    case encoder of
        Opaque toValue ->
            Opaque (f >> toValue)

        Object toValuePairs ->
            toValuePairs
                |> List.map (Tuple.mapSecond (\toValue -> f >> toValue))
                |> Object


at : List String -> Encoder a -> Encoder a
at keyPath encoder =
    case keyPath of
        [] ->
            encoder

        key :: rest ->
            object [ ( key, encodeValue (at rest encoder) ) ]
