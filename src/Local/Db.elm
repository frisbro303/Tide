port module Local.Db exposing (insertOp, opsLoaded, requestOps)

import Json.Decode as Decode
import Json.Encode as Encode
import Ops.Op as Op exposing (Op)


port insertOpPort : Encode.Value -> Cmd msg


port requestOpsPort : () -> Cmd msg


port opsLoadedPort : (Decode.Value -> msg) -> Sub msg


insertOp : Op -> Cmd msg
insertOp op =
    insertOpPort (Op.encoder op)


requestOps : Cmd msg
requestOps =
    requestOpsPort ()


opsLoaded : (List Op -> msg) -> Sub msg
opsLoaded toMsg =
    opsLoadedPort
        (\value ->
            Decode.decodeValue (Decode.list Op.decoder) value
                |> Result.withDefault []
                |> toMsg
        )
