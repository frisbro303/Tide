module Page exposing (Page(..), icon, label, shortcutLabel, toggleable)

import Html exposing (Html)
import List.Extra
import LucideIcons
import Svg.Attributes exposing (height, width)


type Page
    = Review
    | Add
    | Stats
    | Account
    | Settings


toggleable : List Page
toggleable =
    [ Add, Stats, Account, Settings ]


label : Page -> String
label page =
    case page of
        Review ->
            "Review"

        Add ->
            "Add"

        Stats ->
            "Stats"

        Account ->
            "Account"

        Settings ->
            "Settings"


shortcutLabel : Page -> Maybe String
shortcutLabel page =
    toggleable
        |> List.Extra.elemIndex page
        |> Maybe.map (\i -> "⌘" ++ String.fromInt (i + 1))


icon : Page -> Html msg
icon page =
    let
        render =
            case page of
                Review ->
                    LucideIcons.checkCircleIcon

                Add ->
                    LucideIcons.plusIcon

                Stats ->
                    LucideIcons.barChart3Icon

                Account ->
                    LucideIcons.userRoundIcon

                Settings ->
                    LucideIcons.settings2Icon
    in
    render [ width "20", height "20" ]
