module Sync.Sync exposing (appendOps, fetchOps)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Ops.Op as Op
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Sync.Config exposing (anonKey, supabaseUrl)
import Sync.Session exposing (Session)


fetchOps : Session -> (Result Http.Error OpsLog -> msg) -> Cmd msg
fetchOps session toMsg =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "apikey" anonKey
            , Http.header "Authorization" ("Bearer " ++ session.accessToken)
            ]
        , url = supabaseUrl ++ "/rest/v1/ops_log?select=*"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.map OpsLog.fromList (Decode.list Op.decoder))
        , timeout = Nothing
        , tracker = Nothing
        }


appendOps : Session -> OpsLog -> (Result Http.Error () -> msg) -> Cmd msg
appendOps session ops toMsg =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "apikey" anonKey
            , Http.header "Authorization" ("Bearer " ++ session.accessToken)
            , Http.header "Prefer" "resolution=ignore-duplicates"
            ]
        , url = supabaseUrl ++ "/rest/v1/ops_log"
        , body = Http.jsonBody (Encode.list Op.encoder (OpsLog.toList ops))
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }
