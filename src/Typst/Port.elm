port module Typst.Port exposing
    ( compileTypst
    , focusField
    , highlightTypst
    , typstCompiled
    , typstHighlighted
    )

import Json.Decode as Decode


port focusField : String -> Cmd msg


port compileTypstPort : ( String, String, String ) -> Cmd msg


port rawTypstCompiledPort : (( String, Int, String ) -> msg) -> Sub msg


compileTypst : String -> String -> String -> Cmd msg
compileTypst requestId preamble source =
    compileTypstPort ( requestId, source, preamble )


typstCompiled : (String -> Result String String -> msg) -> Sub msg
typstCompiled toMsg =
    rawTypstCompiledPort
        (\( requestId, status, output ) ->
            if status == 0 then
                toMsg requestId (Ok output)

            else
                toMsg requestId (Err output)
        )


port highlightTypstPort : ( String, String ) -> Cmd msg


port typstHighlightedPort : (( String, Decode.Value ) -> msg) -> Sub msg


highlightTypst : String -> String -> Cmd msg
highlightTypst requestId source =
    highlightTypstPort ( requestId, source )


typstHighlighted : (String -> Decode.Value -> msg) -> Sub msg
typstHighlighted toMsg =
    typstHighlightedPort (\( requestId, tree ) -> toMsg requestId tree)
