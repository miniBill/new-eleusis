module View exposing (viewCard, viewSelectableCard)

import Element exposing (Attribute, Color, Element, centerX, column, el, height, padding, pointer, px, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import List.Extra as List
import Model exposing (Card, CardValue(..), Suit(..))
import Theme


viewSelectableCard : Int -> List Card -> Card -> Element (List Card)
viewSelectableCard maxSelection selected card =
    let
        attrs =
            case List.elemIndex card selected of
                Just i ->
                    [ Element.inFront <|
                        el
                            [ Element.centerX
                            , Element.centerY
                            , Background.color Theme.inactive
                            , padding 3
                            , Border.rounded 3
                            , Font.color Theme.black
                            ]
                        <|
                            text <|
                                String.fromInt (i + 1)
                    , pointer
                    , Element.mouseOver [ Background.color Theme.inactive ]
                    , Background.color Theme.blueHighlight
                    , Events.onClick <| List.filter ((/=) card) selected
                    ]

                Nothing ->
                    if List.length selected >= maxSelection then
                        []

                    else
                        [ pointer
                        , Element.mouseOver [ Background.color Theme.blueHighlight ]
                        , Events.onClick <| selected ++ [ card ]
                        ]
    in
    viewCard attrs card


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
