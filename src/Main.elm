module Main exposing (main)

import Browser
import Element
import Html exposing (Html)
import Play
import Setup


type Msg
    = SetupMsg Setup.Msg
    | StartPlaying Play.Flags
    | PlayMsg Play.Msg


type Model
    = SetupModel Setup.Model
    | PlayModel Play.Model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    SetupModel Setup.init


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( SetupMsg msg_, SetupModel model_ ) ->
            SetupModel <| Setup.update msg_ model_

        ( PlayMsg msg_, PlayModel model_ ) ->
            PlayModel <| Play.update msg_ model_

        ( StartPlaying flags, _ ) ->
            PlayModel <| Play.init flags

        _ ->
            model


view : Model -> Html Msg
view model =
    Element.layout [] <|
        case model of
            SetupModel model_ ->
                Setup.view { wrap = SetupMsg, done = StartPlaying } model_

            PlayModel model_ ->
                Play.view PlayMsg model_
