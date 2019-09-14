module Setup exposing (Model, Msg, init, update, view)

import Element exposing (Element, column, el, fill, height, padding, pointer, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty)
import Model exposing (Card, CardValue(..), Dealer, Suit(..))
import Play
import Theme
import View exposing (viewCard)


type Msg
    = Msg Model


type Model
    = RuleWriting RuleWritingModel
    | AddingPlayers AddingPlayersModel


type alias RuleWritingModel =
    { name : String
    , rule : String
    , hint : String
    , firstTurn : List Card
    }


type alias AddingPlayersModel =
    { dealer : Dealer
    , firstTurn : Nonempty Card
    , players : List String
    }


init : Model
init =
    RuleWriting
        { name = ""
        , rule = ""
        , hint = ""
        , firstTurn = []
        }


update : Msg -> Model -> Model
update (Msg model) _ =
    model


view :
    { done : Play.Flags -> msg
    , wrap : Msg -> msg
    }
    -> Model
    -> Element msg
view wrap model =
    column
        [ Background.color Theme.blueBackground
        , spacing 10
        , padding 10
        , width fill
        , height fill
        ]
    <|
        case model of
            RuleWriting r ->
                List.map (Element.map (wrap.wrap << Msg)) <|
                    viewRuleWriting r

            AddingPlayers r ->
                viewAddingPlayers
                    { doneMsg = wrap.done, modelMsg = wrap.wrap << Msg }
                    r


viewRuleWriting : RuleWritingModel -> List (Element Model)
viewRuleWriting model =
    [ text "You are the Dealer"
    , Input.text [ Input.focusedOnLoad ]
        { onChange = \name -> RuleWriting { model | name = name }
        , text = model.name
        , placeholder = Just <| Input.placeholder [] <| text "Enter your name"
        , label = Input.labelAbove [] <| text "Whats your name?"
        }
    , Input.text []
        { onChange = \rule -> RuleWriting { model | rule = rule }
        , text = model.rule
        , placeholder = Just <| Input.placeholder [] <| text "Enter your Rule"
        , label = Input.labelAbove [] <| text "Whats your Rule?"
        }
    , Input.text []
        { onChange = \hint -> RuleWriting { model | hint = hint }
        , text = model.hint
        , placeholder = Just <| Input.placeholder [] <| text "Enter an hint (optional)"
        , label = Input.labelAbove [] <| text "Do you want to give an hint?"
        }
    , text "Initial cards:"
    , Element.wrappedRow [ spacing 10 ] <|
        List.map
            (\v ->
                row [ spacing 10 ] <|
                    List.map
                        (\s ->
                            Element.map (\selected -> RuleWriting { model | firstTurn = selected }) <|
                                View.viewSelectableCard 2 model.firstTurn (Card v s)
                        )
                        [ Hearth, Diamond, Club, Spade ]
            )
            [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]
    ]
        ++ (case ( model.name, model.rule, model.firstTurn ) of
                ( "", _, _ ) ->
                    []

                ( _, "", _ ) ->
                    []

                ( _, _, cards ) ->
                    case Nonempty.fromList cards of
                        Nothing ->
                            []

                        Just cs ->
                            [ Input.button [ Font.color Theme.blueHighlight ]
                                { onPress =
                                    Just <|
                                        AddingPlayers
                                            { dealer = { name = model.name, rule = model.rule, hint = model.hint }
                                            , firstTurn = cs
                                            , players = []
                                            }
                                , label = text "Done"
                                }
                            ]
           )


viewAddingPlayers :
    { doneMsg : Play.Flags -> msg
    , modelMsg : Model -> msg
    }
    -> AddingPlayersModel
    -> List (Element msg)
viewAddingPlayers { doneMsg, modelMsg } { dealer, firstTurn, players } =
    let
        viewPlayerInput i name =
            Input.text [ Input.focusedOnLoad ]
                { onChange =
                    \n ->
                        modelMsg <|
                            AddingPlayers
                                { dealer = dealer
                                , players = List.filter (not << String.isEmpty) <| List.setAt i n <| players ++ [ "" ]
                                , firstTurn = firstTurn
                                }
                , text = name
                , placeholder = Just <| Input.placeholder [] <| text "Enter your name"
                , label = Input.labelHidden "Whats your name?"
                }
    in
    [ text <| "Dealer: " ++ dealer.name
    , text "Players:"
    ]
        ++ List.indexedMap
            viewPlayerInput
            (if List.last players == Just "" then
                players

             else
                players ++ [ "" ]
            )
        ++ [ Input.button [ Font.color Theme.blueHighlight ]
                { onPress =
                    Just <|
                        doneMsg
                            { dealer = dealer
                            , firstTurn = firstTurn
                            , players = List.filter (not << String.isEmpty) players
                            }
                , label = text "Done"
                }
           ]
