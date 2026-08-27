module Typst.Highlight exposing (Node, decoder, view)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode



-- Decodes the tree produced by the Rust `highlight_typst` command, which
-- mirrors `typst_syntax::highlight::highlight_html`'s own recursion exactly
-- (real parser output, not a regex guess) — tags can nest (e.g. `Strong`
-- inside `Heading`), so this is a tree, not a flat token list.


type Node
    = Leaf (Maybe String) String
    | Branch (Maybe String) (List Node)


decoder : Decode.Decoder Node
decoder =
    Decode.map3 (\tag maybeText children -> ( tag, maybeText, children ))
        (Decode.field "tag" (Decode.nullable Decode.string))
        (Decode.field "text" (Decode.nullable Decode.string))
        (Decode.field "children" (Decode.list (Decode.lazy (\_ -> decoder))))
        |> Decode.map
            (\( tag, maybeText, children ) ->
                case maybeText of
                    Just leafText ->
                        Leaf tag leafText

                    Nothing ->
                        Branch tag children
            )


view : Node -> Html msg
view node =
    case node of
        Leaf (Just cls) leafText ->
            span [ class cls ] [ text leafText ]

        Leaf Nothing leafText ->
            text leafText

        Branch tag children ->
            let
                rendered =
                    List.map view children
            in
            case tag of
                Just cls ->
                    span [ class cls ] rendered

                Nothing ->
                    span [] rendered
