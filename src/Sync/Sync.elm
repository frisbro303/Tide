module Sync.Sync exposing (appendOp, fetchOps)

import Http
import Json.Decode as Decode
import Ops.Op as Op exposing (Op)
import Sync.Config exposing (anonKey, supabaseUrl)
import Sync.Session exposing (Session)


fetchOps : Session -> (Result Http.Error (List Op) -> msg) -> Cmd msg
fetchOps session toMsg =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "apikey" anonKey
            , Http.header "Authorization" ("Bearer " ++ session.accessToken)
            ]
        , url = supabaseUrl ++ "/rest/v1/ops_log?select=*"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list Op.decoder)
        , timeout = Nothing
        , tracker = Nothing
        }


appendOp : Session -> Op -> (Result Http.Error () -> msg) -> Cmd msg
appendOp session op toMsg =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "apikey" anonKey
            , Http.header "Authorization" ("Bearer " ++ session.accessToken)
            ]
        , url = supabaseUrl ++ "/rest/v1/ops_log"
        , body = Http.jsonBody (Op.encoder op)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }
