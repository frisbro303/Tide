port module Typst.Port exposing
    ( compileTypst
    , typstCompiled
    )


port compileTypst : String -> Cmd msg


port rawTypstCompiled : (( Int, String ) -> msg) -> Sub msg


typstCompiled : (Result String String -> msg) -> Sub msg
typstCompiled toMsg =
    rawTypstCompiled
        (\( status, output ) ->
            if status == 0 then
                toMsg (Ok output)

            else
                toMsg (Err output)
        )
