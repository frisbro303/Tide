port module Local.Store exposing (delete, get, loaded, set)

import Json.Decode as Decode
import Json.Encode as Encode


port setPort : { key : String, value : Encode.Value } -> Cmd msg


port deletePort : String -> Cmd msg


port getPort : String -> Cmd msg


port loadedPort : ({ key : String, value : Decode.Value } -> msg) -> Sub msg


set : String -> Encode.Value -> Cmd msg
set key value =
    setPort { key = key, value = value }


delete : String -> Cmd msg
delete key =
    deletePort key


get : String -> Cmd msg
get key =
    getPort key


loaded : (String -> Decode.Value -> msg) -> Sub msg
loaded toMsg =
    loadedPort (\{ key, value } -> toMsg key value)
