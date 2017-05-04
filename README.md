# jamesmacaulay/elm-json-bidirectional

This package lets you build up two-way `Coder` structures that concisely specify how values of some Elm type can be both encoded to and decoded from JSON.

## Why?

If you frequently encode from and decode to the same Elm types, it can be tedious and error prone to define your encoders and decoders separately:

```elm
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

type alias User =
    { name : String
    , isAdmin : Bool
    }

userDecoder : Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "name", Encode.string user.name )
        , ( "isAdmin", Encode.bool user.isAdmin )
        ]
```

If you're encoding and decoding a lot of different kinds of data, this requires a lot of code in different functions that needs to be kept in sync. If you add a field to one of your record types but forget to add it to the type's encoder, the compiler can't help you find the omission and you might end up with bad data that can't be decoded with the corresponding decoder. [Fuzz tests work well to prevent this](https://www.brianthicks.com/post/2017/04/24/add-safety-to-your-elm-json-encoders-with-fuzz-testing/), but they require yet more code to be written to remedy the problem.

With this package, you can instead build up a single `Coder` that knows how to both encode and decode:

```elm
import Json.Bidirectional as Json

type alias User =
    { name : String
    , isAdmin : Bool
    }

userCoder : Json.Coder User
userCoder =
    Json.object User
        |> Json.withField "name" .name Json.string
        |> Json.withField "isAdmin" .isAdmin Json.bool

userDecoder : Decoder User
userDecoder =
    Json.decoder userCoder

encodeUser : User -> Encode.Value
encodeUser user =
    Json.encodeValue userCoder user
```

## What's the catch?

Because of the nature of the encoding and decoding processes, this approach is not so great if you are working with JSON that is of a very different structure than its corresponding Elm types.

Also, specifying a bidirectional `Coder` for union types with more than one constructor is a bit of a hassle (see the `custom` function for an example).

## When should I use this package then?

This package is at its best when you have full control over the shape of the JSON that you're encoding and decoding from.

## What about fuzz tests?

Fuzz tests are a great way to make sure your encoders and decoders are mirror images of each other. Here's a great article on the topic, and the inspiration for releasing this package:

https://www.brianthicks.com/post/2017/04/24/add-safety-to-your-elm-json-encoders-with-fuzz-testing/

If you use this package to build bidirectional Coders, you won't need as many fuzz tests to ensure consistency, but you will still want them in some cases where it's possible to make mistakes. The Elm compiler will ensure that values encoded by a Coder will be able to be decoded to the original type by the same Coder, but the type system cannot always guarantee that the decoded _value_ will be identical to the original. Listed below are some ways that you can make asymmetrical Coders with this package if you aren't careful. These are situations where you might decide that fuzz tests are still worthwhile.

## Ordering object fields

One way that the encoded and decoded values might not be equal is if you specify object fields out of order. For example:

```elm
import Json.Bidirectional as Json

type alias EmailContact =
    { name : String
    , email : String
    }

emailContactCoder : Json.Coder EmailContact
emailContactCoder =
    Json.object EmailContact
        -- fields in the wrong order!
        |> Json.withField "email" .email Json.string
        |> Json.withField "name" .name Json.string
```

The above Coder will encode `{ name = "Alice", email = "alice@example.com" }` correctly as `{"name": "Alice", "email", "alice@example.com"}`. However, because the two string fields are specified in the wrong order, the EmailContact constructor decodes the `"email"` field as its `name` and vice-versa.

## bimap

The `bimap` function lets you map both the encoding and decoding processes of a Coder by supplying one function for each direction. Here's a contrived example:

```elm
import Json.Bidirectional as Json

type StringPair
    = StringPair String String

stringPairCoder : Json.Coder StringPair
stringPairCoder =
    Json.tuple (Json.string, Json.string)
        |> Json.bimap
            (\(StringPair left right) -> (left, right))
            (\(left, right) -> StringPair left right)
```

These mapping functions are just complex enough that you might make a mistake in the implementation:

```elm
inconsistentStringPairCoder : Json.Coder StringPair
inconsistentStringPairCoder =
    Json.tuple (Json.string, Json.string)
        |> Json.bimap
            -- the left String is used in both places in the encoding!
            (\(StringPair left right) -> (left, left))
            (\(left, right) -> StringPair left right)
```

## custom

The `custom` function lets you create an arbitrary Coder for any type by supplying an encoding function and Decoder for a single type. This function is most useful for implementing Coders for union types with multiple constructors. Use of the `custom` function in this way tends to be the most complex and error-prone way of constructing a Coder that this package makes available, and so fuzz testing `custom` Coders is highly recommended.
