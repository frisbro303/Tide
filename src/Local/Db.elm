port module Local.Db exposing (clearOps, insertOp, insertOps, opsLoaded, requestOps)

import Json.Decode as Decode
import Json.Encode as Encode
import Ops.Op as Op exposing (Op)
import Ops.OpsLog as OpsLog exposing (OpsLog)


port insertOpsPort : Encode.Value -> Cmd msg


port requestOpsPort : () -> Cmd msg


port clearOpsPort : () -> Cmd msg


port opsLoadedPort : (Decode.Value -> msg) -> Sub msg


insertOp : Op -> Cmd msg
insertOp op =
    insertOps (OpsLog.fromList [ op ])


insertOps : OpsLog -> Cmd msg
insertOps ops =
    insertOpsPort (Encode.list Op.encoder (OpsLog.toList ops))


requestOps : Cmd msg
requestOps =
    requestOpsPort ()


clearOps : Cmd msg
clearOps =
    clearOpsPort ()


opsLoaded : (OpsLog -> msg) -> Sub msg
opsLoaded toMsg =
    opsLoadedPort
        (\value ->
            Decode.decodeValue (Decode.list Op.decoder) value
                |> Result.withDefault []
                |> OpsLog.fromList
                |> toMsg
        )
