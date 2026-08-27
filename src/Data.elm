port module Data exposing (exportData, importLoaded, requestImport)

import Json.Encode as Encode


port exportDataPort : String -> Cmd msg


port requestImportPort : () -> Cmd msg


port importLoadedPort : (String -> msg) -> Sub msg


exportData : Encode.Value -> Cmd msg
exportData value =
    exportDataPort (Encode.encode 0 value)


requestImport : Cmd msg
requestImport =
    requestImportPort ()


importLoaded : (String -> msg) -> Sub msg
importLoaded =
    importLoadedPort
