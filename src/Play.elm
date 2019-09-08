module Play exposing (Flags, Model, Msg, init, update, view)

import AdorableAvatar
import Bitwise
import Element exposing (Attribute, Color, Element, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, paddingXY, pointer, px, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Model exposing (Card, CardValue(..), Dealer, Player, Suit(..))
import Random
import Sha256
import Theme
import View exposing (viewCard)


type alias Flags =
    { dealer : Dealer
    , players : List String
    , firstTurn : Nonempty Card
    }


type Msg
    = PrepareCard Card
    | UnprepareCard Card
    | DonePreparingPlay
    | DonePreparingNoPlay
    | GoodPlay (Nonempty Card)
    | BadPlay (Nonempty Card)
    | GoodNoPlay (Nonempty Card)
    | BadNoPlay (Nonempty Card)


type alias Model =
    { dealer : Dealer
    , prophet : Maybe Player
    , players : List Player
    , playedTurns : Nonempty PlayedTurn
    , currentTurn : CurrentTurn
    }


type CurrentTurn
    = Preparing (List Card)
    | Playing (Nonempty Card)
    | NoPlaying (Nonempty Card)


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

        suitGenerator =
            Random.uniform Hearth [ Diamond, Club, Spade ]

        valueGenerator =
            Random.uniform Ace [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]

        cardGenerator =
            Random.map2 Card valueGenerator suitGenerator

        handGenerator =
            Random.list 14 cardGenerator

        initialSeed =
            toShaInt flags.dealer.rule

        players =
            Tuple.second <|
                List.foldl
                    (\name ( seed, list ) ->
                        let
                            ( hand, seed_ ) =
                                Random.step handGenerator seed
                        in
                        ( seed_, { name = name, hand = hand } :: list )
                    )
                    ( Random.initialSeed initialSeed, [] )
                    playerNames

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
    }


nextTurn : List Player -> List Player
nextTurn ps =
    case ps of
        [] ->
            []

        p :: players ->
            if List.isEmpty p.hand then
                players

            else
                players ++ [ p ]


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
    case msg of
        PrepareCard card ->
            case model.players of
                player :: players ->
                    let
                        players_ =
                            { player | hand = removeFirst card player.hand } :: players
                    in
                    { model
                        | players = players_
                        , currentTurn =
                            case model.currentTurn of
                                Preparing ne ->
                                    Preparing <| ne ++ [ card ]

                                _ ->
                                    model.currentTurn
                    }

                [] ->
                    model

        UnprepareCard card ->
            case model.players of
                player :: players ->
                    let
                        players_ =
                            { player | hand = player.hand ++ [ card ] } :: players
                    in
                    { model
                        | players = players_
                        , currentTurn =
                            case model.currentTurn of
                                Preparing ne ->
                                    Preparing <| removeFirst card ne

                                _ ->
                                    model.currentTurn
                    }

                [] ->
                    model

        GoodPlay cards ->
            { model
                | players = nextTurn model.players
                , playedTurns =
                    Nonempty.append
                        (Nonempty.reverse <| Nonempty.map played cards)
                        model.playedTurns
                , currentTurn = Preparing []
            }

        BadPlay cards ->
            let
                turn =
                    let
                        last =
                            Nonempty.head model.playedTurns

                        wrong_ =
                            last.wrong ++ [ { cards = cards, type_ = Wrong } ]
                    in
                    { last | wrong = wrong_ }
            in
            { model
                | players = nextTurn model.players
                , playedTurns = Nonempty.replaceHead turn model.playedTurns
                , currentTurn = Preparing []
            }

        GoodNoPlay cards ->
            let
                turn =
                    let
                        last =
                            Nonempty.head model.playedTurns

                        wrong_ =
                            last.wrong ++ [ { cards = cards, type_ = NoPlay } ]
                    in
                    { last | wrong = wrong_ }
            in
            { model
                | players = nextTurn model.players
                , playedTurns = Nonempty.replaceHead turn model.playedTurns
                , currentTurn = Preparing []
            }

        BadNoPlay cards ->
            let
                turn =
                    let
                        last =
                            Nonempty.head model.playedTurns

                        wrong_ =
                            last.wrong ++ [ { cards = cards, type_ = Wrong } ]
                    in
                    { last | wrong = wrong_ }
            in
            { model
                | players = nextTurn model.players
                , playedTurns = Nonempty.replaceHead turn model.playedTurns
                , currentTurn = Preparing []
            }

        DonePreparingPlay ->
            case model.currentTurn of
                Preparing (x :: xs) ->
                    { model | currentTurn = Playing <| Nonempty x xs }

                _ ->
                    model

        DonePreparingNoPlay ->
            case model.currentTurn of
                Preparing (x :: xs) ->
                    { model | currentTurn = NoPlaying <| Nonempty x xs }

                _ ->
                    model


view : (Msg -> msg) -> Model -> Element msg
view mapper model =
    Element.map mapper <|
        column [ width fill, height fill ]
            [ viewGameInfo model
            , viewControls model
            , viewBoard model
            , row [ height fill, width fill, Background.color Theme.inactive ] []
            ]


viewGameInfo : Model -> Element Msg
viewGameInfo { currentTurn, dealer, prophet, players } =
    column
        [ width fill
        , padding 10
        , Background.color Theme.inactive
        ]
        [ wrappedRow []
            [ text
                "Current prophet: "
            , Maybe.withDefault (text "none") (Maybe.map viewPlayer prophet)
            ]
        , wrappedRow
            (case currentTurn of
                Preparing _ ->
                    []

                _ ->
                    [ Font.bold
                    , Background.color Theme.blueBackground
                    ]
            )
            [ text "Current dealer: ", viewPlayer dealer ]
        , wrappedRow [] <|
            [ text "Next turns: " ]
                ++ List.intersperse (text ", ")
                    (List.map viewPlayer <| List.drop 1 players)
        ]


viewPlayer : { a | name : String } -> Element msg
viewPlayer { name } =
    row [ spacing 10, padding 10 ]
        [ Element.image
            [ width <| px 30
            , height <| px 30
            ]
            { src = AdorableAvatar.url name
            , description = ""
            }
        , text name
        ]


viewControls : Model -> Element Msg
viewControls model =
    case model.players of
        [] ->
            column [ width fill, Background.color Theme.good, padding 10 ]
                [ el [ Font.bold, Font.size 40 ] <|
                    text "No more cards! Game done!"
                ]

        player :: _ ->
            let
                playerHand =
                    text "Your hand: "
                        :: List.map
                            (\card ->
                                viewCard
                                    (case model.currentTurn of
                                        Preparing _ ->
                                            [ pointer
                                            , Element.mouseOver [ Background.color Theme.blueHighlight ]
                                            , Events.onClick <| PrepareCard card
                                            ]

                                        _ ->
                                            []
                                    )
                                    card
                            )
                            player.hand

                playButtons =
                    [ column
                        [ height <| px 89 ]
                        [ row
                            [ Background.color Theme.good
                            , Element.mouseOver [ Background.color Theme.veryGood ]
                            , width fill
                            , height fill
                            , padding 10
                            , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
                            , Border.color Theme.black
                            , Border.widthEach { left = 1, top = 1, right = 1, bottom = 0 }
                            , pointer
                            , Events.onClick DonePreparingPlay
                            ]
                            [ el [ centerX, centerY ] <| text "Play!" ]
                        , row
                            [ Background.color Theme.bad
                            , Element.mouseOver [ Background.color Theme.veryBad ]
                            , width fill
                            , height fill
                            , padding 10
                            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
                            , Border.color Theme.black
                            , Border.widthEach { left = 1, top = 0, right = 1, bottom = 1 }
                            , pointer
                            , Events.onClick DonePreparingNoPlay
                            ]
                            [ el [ centerX, centerY ] <| text "No Play!" ]
                        ]
                    ]
            in
            column
                [ width fill
                , Background.color <|
                    case model.currentTurn of
                        Preparing _ ->
                            Theme.blueBackground

                        _ ->
                            Theme.inactive
                , padding 10
                ]
                [ row
                    (case model.currentTurn of
                        Preparing _ ->
                            [ Font.bold ]

                        _ ->
                            []
                    )
                    [ text "Current player: ", viewPlayer player ]
                , case model.currentTurn of
                    Preparing [] ->
                        wrappedRow [ paddingXY 0 10, spacing 10 ] playerHand

                    Preparing (_ :: _) ->
                        wrappedRow [ paddingXY 0 10, spacing 10 ] (playerHand ++ playButtons)

                    _ ->
                        Element.none
                ]


viewBoard : Model -> Element Msg
viewBoard model =
    column [ Background.color Theme.inactive, width fill ]
        [ text "Board:"
        , wrappedRow
            [ width fill
            , paddingEach { left = 10, top = 0, right = 0, bottom = 0 }
            , centerY
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

        maybePlaying color cards ( goodMsg, goodLabel ) ( badMsg, badLabel ) =
            deck color <|
                List.map
                    (viewCard [])
                    (Nonempty.toList cards)
                    ++ [ el
                            [ paddingEach { left = 44, top = 0, right = 0, bottom = 0 } ]
                         <|
                            column
                                [ height <| px 89 ]
                                [ row
                                    [ Background.color Theme.good
                                    , Element.mouseOver [ Background.color Theme.veryGood ]
                                    , width fill
                                    , height fill
                                    , padding 10
                                    , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
                                    , Border.color Theme.black
                                    , Border.widthEach { left = 1, top = 1, right = 1, bottom = 0 }
                                    , pointer
                                    , Events.onClick <| goodMsg cards
                                    ]
                                    [ el [ centerX, centerY ] <| text goodLabel ]
                                , row
                                    [ Background.color Theme.bad
                                    , Element.mouseOver [ Background.color Theme.veryBad ]
                                    , width fill
                                    , height fill
                                    , padding 10
                                    , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
                                    , Border.color Theme.black
                                    , Border.widthEach { left = 1, top = 0, right = 1, bottom = 1 }
                                    , pointer
                                    , Events.onClick <| badMsg cards
                                    ]
                                    [ el [ centerX, centerY ] <| text badLabel ]
                                ]
                       ]
    in
    case turn of
        Preparing cards ->
            if List.isEmpty cards then
                Element.none

            else
                deck Theme.blueBackground <|
                    List.map
                        (\card ->
                            viewCard
                                [ Element.mouseOver [ Background.color Theme.blueHighlight ]
                                , pointer
                                , Events.onClick <| UnprepareCard card
                                ]
                                card
                        )
                        cards

        Playing cards ->
            maybePlaying Theme.blueBackground
                cards
                ( GoodPlay, "Play" )
                ( BadPlay, "Error" )

        NoPlaying cards ->
            maybePlaying Theme.bad
                cards
                ( GoodNoPlay, "No Play" )
                ( BadNoPlay, "Error" )


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
