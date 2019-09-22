module View exposing (selectableAttrs, viewCard, viewControlsCards, viewSelectableCard)

import Element exposing (Attribute, Color, Element, centerX, centerY, column, el, fill, height, mouseOver, padding, pointer, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import List.Extra as List
import Model exposing (Card, CardValue(..), Suit(..))
import Theme


selectableAttrs : msg -> List (Attribute msg)
selectableAttrs msg =
    [ pointer
    , mouseOver [ Background.color Theme.blueHighlight ]
    , Events.onClick msg
    ]


viewSelectableCard : Int -> List Card -> Card -> Element (List Card)
viewSelectableCard maxSelection selected card =
    let
        attrs =
            case List.elemIndex card selected of
                Just i ->
                    [ Element.inFront <|
                        el
                            [ centerX
                            , centerY
                            , Background.color Theme.inactive
                            , padding 3
                            , Border.rounded 3
                            , Font.color Theme.black
                            ]
                        <|
                            text <|
                                String.fromInt (i + 1)
                    , pointer
                    , mouseOver [ Background.color Theme.inactive ]
                    , Background.color Theme.blueHighlight
                    , Events.onClick <| List.filter ((/=) card) selected
                    ]

                Nothing ->
                    if List.length selected >= maxSelection then
                        []

                    else
                        selectableAttrs <| selected ++ [ card ]
    in
    viewCard attrs card


viewControlsCards : List { color : Color, highlight : Color, msg : msg, text : String } -> Element msg
viewControlsCards controls =
    let
        toCard group =
            let
                toSegment : Int -> { color : Color, highlight : Color, msg : msg, text : String } -> Element msg
                toSegment i { color, highlight, msg, text } =
                    let
                        common =
                            [ Background.color color
                            , mouseOver [ Background.color highlight ]
                            , width fill
                            , height fill
                            , padding 5
                            , Border.color Theme.black
                            , pointer
                            , Events.onClick msg
                            , Font.size 16
                            ]

                        specific =
                            if List.length group == 1 then
                                [ Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 5, bottomRight = 5 }
                                , Border.widthEach { left = 1, top = 1, right = 1, bottom = 1 }
                                ]

                            else if i == 0 then
                                [ Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
                                , Border.widthEach { left = 1, top = 1, right = 1, bottom = 0 }
                                ]

                            else if i == List.length group - 1 then
                                [ Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
                                , Border.widthEach { left = 1, top = 0, right = 1, bottom = 1 }
                                ]

                            else
                                [ Border.widthEach { left = 1, top = 0, right = 1, bottom = 0 }
                                ]
                    in
                    row (common ++ specific)
                        [ Element.el [ centerX, centerY ] <| Element.text text ]
            in
            column
                [ width <| px 64
                , height <| px 89
                ]
            <|
                List.indexedMap toSegment group
    in
    controls
        |> balancedGroupsOf 3
        |> List.map toCard
        |> row [ spacing 10 ]


balancedGroupsOf : Int -> List a -> List (List a)
balancedGroupsOf size list =
    let
        len =
            List.length list

        groupCount =
            (len - 1) // size + 1

        actualSize =
            (len - 1) // groupCount + 1
    in
    List.greedyGroupsOf actualSize list


viewCard : List (Attribute msg) -> Card -> Element msg
viewCard attrs card =
    let
        defaultAttrs =
            [ Border.width 1
            , Border.rounded 5
            , Border.color Theme.black
            , Background.color Theme.white
            , width <| px 64
            , height <| px 89
            , Element.paddingXY 2 2
            , Font.color <| cardToColor card
            , Font.size 16
            ]

        ul =
            column []
                [ el [ centerX ] <| text <| valueToString card.value
                , el [ centerX ] <| text <| suitToString card.suit
                ]

        br =
            el
                [ Element.alignBottom
                , Element.alignRight
                , Element.rotate pi
                ]
                ul
    in
    column
        (defaultAttrs ++ attrs)
        [ ul, br ]


cardToColor : Card -> Color
cardToColor { suit } =
    case suit of
        Hearth ->
            rgb255 160 0 0

        Diamond ->
            rgb255 160 0 0

        Club ->
            Theme.black

        Spade ->
            Theme.black


suitToString : Suit -> String
suitToString suit =
    case suit of
        Hearth ->
            "♥"

        Diamond ->
            "♦"

        Club ->
            "♣"

        Spade ->
            "♠"


valueToString : CardValue -> String
valueToString value =
    case value of
        Ace ->
            "A"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"
