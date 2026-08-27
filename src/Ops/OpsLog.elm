module Ops.OpsLog exposing (OpsLog, diff, emptyOpsLog, foldl, fromList, insert, merge, size, toList)

import Dict exposing (Dict)
import Ops.Op exposing (Op, OpId(..))
import Time
import UUID


type OpsLog
    = OpsLog (Dict ( Int, String ) Op)


opKey : Op -> ( Int, String )
opKey op =
    let
        (OpId uuid) =
            op.id
    in
    ( Time.posixToMillis op.timeStamp, UUID.toString uuid )


emptyOpsLog : OpsLog
emptyOpsLog =
    OpsLog Dict.empty


insert : Op -> OpsLog -> OpsLog
insert op (OpsLog dict) =
    OpsLog (Dict.insert (opKey op) op dict)


fromList : List Op -> OpsLog
fromList ops =
    List.foldl insert emptyOpsLog ops


toList : OpsLog -> List Op
toList (OpsLog dict) =
    Dict.values dict


size : OpsLog -> Int
size (OpsLog dict) =
    Dict.size dict


foldl : (Op -> a -> a) -> a -> OpsLog -> a
foldl step initial (OpsLog dict) =
    Dict.foldl (\_ op acc -> step op acc) initial dict


merge : OpsLog -> OpsLog -> OpsLog
merge (OpsLog a) (OpsLog b) =
    OpsLog (Dict.union a b)


diff : OpsLog -> OpsLog -> OpsLog
diff (OpsLog a) (OpsLog b) =
    OpsLog (Dict.diff a b)
