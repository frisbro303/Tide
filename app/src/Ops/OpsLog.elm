module Ops.OpsLog exposing (..)

import List.Extra
import Ops.Op exposing (Op)
import Time



-- Conflict-free because card IDs are unique, updates target only
-- observed cards, reviews are immutable facts, and edits are last write win.


type alias OpsLog =
    List Op


emptyOpsLog : OpsLog
emptyOpsLog =
    []


append : Op -> OpsLog -> OpsLog
append op log =
    op :: log


uniqueOps : OpsLog -> OpsLog
uniqueOps =
    List.Extra.uniqueBy .id


sortOps : OpsLog -> OpsLog
sortOps =
    List.sortBy (\op -> Time.posixToMillis op.timeStamp)


merge : OpsLog -> OpsLog -> OpsLog
merge a b =
    (a ++ b) |> uniqueOps |> sortOps
