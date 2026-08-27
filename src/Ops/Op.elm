module Ops.Op exposing (Op, OpId(..), OpKind(..), decoder, encoder)

import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Sea.Card exposing (CardId)
import Sea.FSRS exposing (Rating(..))
import Time exposing (Posix)
import UUID exposing (UUID)


type alias Op =
    { id : OpId
    , timeStamp : Posix
    , opKind : OpKind
    }


type OpId
    = OpId UUID


type OpKind
    = CreateCard
        { id : CardId
        , front : String
        , back : String
        }
    | EditCard
        { id : CardId
        , front : String
        , back : String
        }
    | DeleteCard CardId
    | ReviewCard
        { id : CardId
        , rating : Rating
        }
    | SetPreamble String


decoder : Decode.Decoder Op
decoder =
    Decode.map3 Op
        (Decode.field "id" uuidDecoder |> Decode.map OpId)
        (Decode.field "created_at" Iso8601.decoder)
        (Decode.field "entry" opKindDecoder)


uuidDecoder : Decode.Decoder UUID
uuidDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case UUID.fromString str of
                    Ok uuid ->
                        Decode.succeed uuid

                    Err _ ->
                        Decode.fail ("Invalid UUID: " ++ str)
            )


opKindDecoder : Decode.Decoder OpKind
opKindDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "CreateCard" ->
                        Decode.map3
                            (\id front back -> CreateCard { id = id, front = front, back = back })
                            (Decode.field "id" uuidDecoder)
                            (Decode.field "front" Decode.string)
                            (Decode.field "back" Decode.string)

                    "EditCard" ->
                        Decode.map3
                            (\id front back -> EditCard { id = id, front = front, back = back })
                            (Decode.field "id" uuidDecoder)
                            (Decode.field "front" Decode.string)
                            (Decode.field "back" Decode.string)

                    "DeleteCard" ->
                        Decode.map DeleteCard (Decode.field "id" uuidDecoder)

                    "ReviewCard" ->
                        Decode.map2
                            (\id rating -> ReviewCard { id = id, rating = rating })
                            (Decode.field "id" uuidDecoder)
                            (Decode.field "rating" ratingDecoder)

                    "SetPreamble" ->
                        Decode.map SetPreamble (Decode.field "preamble" Decode.string)

                    other ->
                        Decode.fail ("Unknown op kind: " ++ other)
            )


ratingDecoder : Decode.Decoder Rating
ratingDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Again" ->
                        Decode.succeed Again

                    "Hard" ->
                        Decode.succeed Hard

                    "Good" ->
                        Decode.succeed Good

                    "Easy" ->
                        Decode.succeed Easy

                    other ->
                        Decode.fail ("Unknown rating: " ++ other)
            )


encoder : Op -> Encode.Value
encoder op =
    let
        (OpId uuid) =
            op.id
    in
    Encode.object
        [ ( "id", uuidEncoder uuid )
        , ( "created_at", Iso8601.encode op.timeStamp )
        , ( "entry", opKindEncoder op.opKind )
        ]


opKindEncoder : OpKind -> Encode.Value
opKindEncoder opKind =
    case opKind of
        CreateCard { id, front, back } ->
            Encode.object
                [ ( "kind", Encode.string "CreateCard" )
                , ( "id", uuidEncoder id )
                , ( "front", Encode.string front )
                , ( "back", Encode.string back )
                ]

        EditCard { id, front, back } ->
            Encode.object
                [ ( "kind", Encode.string "EditCard" )
                , ( "id", uuidEncoder id )
                , ( "front", Encode.string front )
                , ( "back", Encode.string back )
                ]

        DeleteCard id ->
            Encode.object
                [ ( "kind", Encode.string "DeleteCard" )
                , ( "id", uuidEncoder id )
                ]

        ReviewCard { id, rating } ->
            Encode.object
                [ ( "kind", Encode.string "ReviewCard" )
                , ( "id", uuidEncoder id )
                , ( "rating", ratingEncoder rating )
                ]

        SetPreamble preamble ->
            Encode.object
                [ ( "kind", Encode.string "SetPreamble" )
                , ( "preamble", Encode.string preamble )
                ]


uuidEncoder : UUID -> Encode.Value
uuidEncoder =
    UUID.toString >> Encode.string


ratingEncoder : Rating -> Encode.Value
ratingEncoder rating =
    Encode.string
        (case rating of
            Again ->
                "Again"

            Hard ->
                "Hard"

            Good ->
                "Good"

            Easy ->
                "Easy"
        )
