port module Theme exposing (Theme(..), fromString, setTheme, toString)


port setThemePort : String -> Cmd msg


type Theme
    = System
    | Light
    | Dark


toString : Theme -> String
toString theme =
    case theme of
        System ->
            "system"

        Light ->
            "light"

        Dark ->
            "dark"


fromString : String -> Theme
fromString raw =
    case raw of
        "light" ->
            Light

        "dark" ->
            Dark

        _ ->
            System


setTheme : Theme -> Cmd msg
setTheme theme =
    setThemePort (toString theme)
