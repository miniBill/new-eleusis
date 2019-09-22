module Play exposing (Flags, Model, Msg, init, update, view)

import AdorableAvatar
import Bitwise
import Element exposing (Attribute, Color, Element, alignTop, centerX, centerY, column, el, fill, height, mouseOver, padding, paddingEach, paddingXY, pointer, px, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import EverySet
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Model exposing (Card, CardValue(..), Dealer, Player, Suit(..))
import Random
import Random.List as Random
import Sha256
import Theme
import View exposing (viewCard, viewSelectableCard)


type alias Flags =
    { dealer : Dealer
    , players : List String
    , firstTurn : Nonempty Card
    }


type Msg
    = PreparingCards (List Card)
    | Play
    | DeclareNoPlay
    | GoodPlay (Nonempty Card)
    | BadPlay { wrong : Int }
    | GoodNoPlay
    | BadNoPlay Card


type alias Model =
    { dealer : Dealer
    , prophet : Maybe Player
    , players : List Player
    , playedTurns : Nonempty PlayedTurn
    , currentTurn : CurrentTurn
    , deck : List Card
    }


type CurrentTurn
    = Preparing (List Card)
    | Playing (Nonempty Card)
    | NoPlaying


type alias PlayedTurn =
    { correct : Card
    , wrong :
        List
            { cards : Nonempty Card
            , type_ : WrongType
            }
    }


type WrongType
    = Wrong
    | NoPlay


played : Card -> PlayedTurn
played correct =
    { correct = correct
    , wrong = []
    }


init : Flags -> Model
init flags =
    let
        playerNames =
            List.reverse <| flags.players

        initialSeed =
            toShaInt flags.dealer.rule

        sortedDeck =
            List.lift3
                Card
                [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]
                [ Hearth, Diamond, Club, Spade ]
                [ 1, 2 ]
                |> List.filter (\card -> not <| Nonempty.member card flags.firstTurn)

        shuffledDeck =
            Random.step (Random.shuffle sortedDeck) (Random.initialSeed initialSeed)
                |> Tuple.first

        playersAndDeck : ( List Player, List Card )
        playersAndDeck =
            playerNames
                |> List.foldl
                    (\name ( ps, dk ) ->
                        let
                            ( hand, dk_ ) =
                                List.splitAt 14 dk
                        in
                        ( { name = name, maybeHand = Nonempty.fromList hand } :: ps, dk_ )
                    )
                    ( [], shuffledDeck )
                |> Tuple.mapFirst (List.filterMap (\{ name, maybeHand } -> Maybe.map (\hand -> { name = name, hand = hand }) maybeHand))

        ( players, deck ) =
            playersAndDeck

        playedTurns =
            flags.firstTurn
                |> Nonempty.map (\c -> { correct = c, wrong = [] })
                |> Nonempty.reverse
    in
    { dealer = flags.dealer
    , prophet = Nothing
    , players = players
    , currentTurn = Preparing []
    , playedTurns = playedTurns
    , deck = deck
    }


nextTurn : Player -> List Player -> List Player
nextTurn p ps =
    if EverySet.isEmpty p.hand then
        ps

    else
        ps ++ [ p ]


removeFirst : a -> List a -> List a
removeFirst c h =
    case h of
        [] ->
            []

        x :: xs ->
            if x == c then
                xs

            else
                x :: removeFirst c xs


update : Msg -> Model -> Model
update msg model =
    case model.players of
        [] ->
            model

        player :: players ->
            case msg of
                PreparingCards cards ->
                    { model | currentTurn = Preparing cards }

                GoodPlay cards ->
                    let
                        player_ =
                            { player | hand = EverySet.diff player.hand <| EverySet.fromList <| Nonempty.toList cards }
                    in
                    { model | players = nextTurn player_ players, currentTurn = Preparing [] }

                BadPlay { wrong } ->
                    model

                GoodNoPlay ->
                    let
                        ( hand_, deck_ ) =
                            List.splitAt (EverySet.size player.hand - 4) model.deck

                        player_ =
                            { player | hand = EverySet.fromList hand_ }

                        addNoPlay : PlayedTurn -> PlayedTurn
                        addNoPlay turn =
                            { turn
                                | wrong = turn.wrong ++ [ { type_ = NoPlay, cards = player.hand } ]
                            }

                        playedTurns_ =
                            model.playedTurns
                                |> Nonempty.replaceHead (model.playedTurns |> Nonempty.head |> addNoPlay)
                    in
                    { model | players = nextTurn player_ players, currentTurn = Preparing [] }

                BadNoPlay cards ->
                    model

                Play ->
                    case model.currentTurn of
                        Preparing (card :: cards) ->
                            { model | currentTurn = Playing <| Nonempty card cards }

                        _ ->
                            model

                DeclareNoPlay ->
                    { model | currentTurn = NoPlaying }


view : (Msg -> msg) -> Model -> Element msg
view mapper model =
    Element.map mapper <|
        column [ width fill, height fill ] <|
            List.concat
                [ viewGameInfo model
                , viewControls model
                , viewBoard model
                , [ mainRow False [ height fill ] [] ]
                ]


viewGameInfo : Model -> List (Element Msg)
viewGameInfo model =
    [ case model.prophet of
        Just prophet ->
            mainRow False
                []
                [ text "Prophet: "
                , viewPlayer prophet
                ]

        Nothing ->
            Element.none
    , mainRow (not <| isPreparing model)
        []
        [ text "Dealer: ", viewPlayer model.dealer ]
    ]


viewPlayer : { a | name : String } -> Element msg
viewPlayer player =
    let
        ( img, name ) =
            if String.isEmpty player.name then
                ( el
                    [ width <| px 30
                    , height <| px 30
                    , Border.width 0
                    , Background.color Theme.white
                    ]
                  <|
                    el
                        [ centerX
                        , centerY
                        ]
                    <|
                        text "∅"
                , "none"
                )

            else
                ( Element.image
                    [ width <| px 30
                    , height <| px 30
                    ]
                    { src = AdorableAvatar.url player.name
                    , description = ""
                    }
                , player.name
                )
    in
    row [ spacing 10 ] [ img, text name ]


mainRow : Bool -> List (Attribute msg) -> List (Element msg) -> Element msg
mainRow active attrs =
    wrappedRow <|
        List.concat
            [ [ width fill, paddingXY 10 5, spacing 10 ]
            , if active then
                [ Background.color Theme.blueBackground ]

              else
                [ Background.color Theme.inactive ]
            , attrs
            ]


isPreparing : Model -> Bool
isPreparing model =
    case model.currentTurn of
        Preparing _ ->
            True

        _ ->
            False


viewControls : Model -> List (Element Msg)
viewControls model =
    case model.players of
        [] ->
            [ mainRow True
                [ Font.bold
                , Font.size 40
                ]
                [ text "No more players! The game is finished!"
                ]
            ]

        player :: _ ->
            let
                playerHand =
                    List.map
                        (case model.currentTurn of
                            Preparing cs ->
                                Element.map PreparingCards << viewSelectableCard 4 cs

                            NoPlaying ->
                                \card ->
                                    viewCard
                                        [ pointer
                                        , mouseOver [ Background.color Theme.bad ]
                                        , Events.onClick <| BadNoPlay card
                                        ]
                                        card

                            Playing _ ->
                                always Element.none
                        )
                        (EverySet.toList player.hand)

                playButton =
                    { color = Theme.good, highlight = Theme.veryGood, msg = Play, text = "Play" }

                noPlayButton =
                    { color = Theme.bad, highlight = Theme.veryBad, msg = DeclareNoPlay, text = "No\nPlay" }

                playButtons cards =
                    [ View.viewControlsCards <|
                        if List.isEmpty cards then
                            [ noPlayButton ]

                        else
                            [ playButton ]
                    ]

                noPlayButtons =
                    [ View.viewControlsCards [ { color = Theme.good, highlight = Theme.veryGood, msg = GoodNoPlay, text = "✓" } ] ]
            in
            [ mainRow (isPreparing model)
                []
                [ text "Plays: ", viewPlayer player ]
            , mainRow True
                []
                (playerHand
                    ++ (case model.currentTurn of
                            Preparing cs ->
                                playButtons cs

                            NoPlaying ->
                                noPlayButtons

                            Playing _ ->
                                []
                       )
                )
            ]


viewBoard : Model -> List (Element Msg)
viewBoard model =
    [ mainRow False [] [ text "Board:" ]
    , mainRow False
        [ centerY
        , spacing 30
        ]
      <|
        List.map viewPlayedTurn (List.reverse <| Nonempty.toList model.playedTurns)
            ++ [ viewCurrentTurn model.currentTurn ]
    ]


viewCurrentTurn : CurrentTurn -> Element Msg
viewCurrentTurn turn =
    let
        deck color rest =
            row
                [ padding 10
                , alignTop
                , Background.color color
                , spacing -36
                ]
                rest

        maybePlaying color cards =
            deck color <|
                List.map
                    (viewCard [])
                    (Nonempty.toList cards)
                    ++ [ el
                            [ paddingEach { left = 44, top = 0, right = 0, bottom = 0 } ]
                         <|
                            View.viewControlsCards <|
                                { color = Theme.good, highlight = Theme.veryGood, msg = GoodPlay cards, text = "✓" }
                                    :: List.map
                                        (\w ->
                                            { color = Theme.bad
                                            , highlight = Theme.veryBad
                                            , msg = BadPlay { wrong = w }
                                            , text = String.fromInt w ++ " ✗"
                                            }
                                        )
                                        (List.range 1 (Nonempty.length cards))
                       ]
    in
    case turn of
        Preparing _ ->
            Element.none

        Playing cards ->
            maybePlaying Theme.blueBackground cards

        NoPlaying ->
            Element.none


viewPlayedTurn : PlayedTurn -> Element msg
viewPlayedTurn turn =
    column [ paddingEach { left = 0, top = 10, right = 0, bottom = 0 }, spacing 10, alignTop ] <|
        [ el [ centerX ] <|
            viewCard
                []
                turn.correct
        ]
            ++ List.map
                (\wrong ->
                    let
                        color =
                            case wrong.type_ of
                                Wrong ->
                                    Theme.bad

                                NoPlay ->
                                    Theme.good
                    in
                    viewCardStack color <| Nonempty.toList wrong.cards
                )
                turn.wrong


viewCardStack : Color -> List Card -> Element msg
viewCardStack color cards =
    row
        [ spacing -36
        , centerX
        , paddingXY 10 0
        , Background.color color
        ]
    <|
        List.indexedMap
            (\i ->
                el
                    [ paddingEach
                        { top = 10 + 4 * i
                        , right = 0
                        , bottom = 10 + 4 * (List.length cards - i - 1)
                        , left = 0
                        }
                    ]
                    << viewCard []
            )
            cards


toShaInt : String -> Int
toShaInt =
    Sha256.sha256
        >> String.toList
        >> List.map Char.toCode
        >> List.foldl (\e a -> Bitwise.xor e (a * 33)) 0
