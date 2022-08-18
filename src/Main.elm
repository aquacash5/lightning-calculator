module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type AppState
    = Base
    | Lightning Time.Posix


type alias Model =
    { curTime : Time.Posix
    , timezone : Time.Zone
    , state : AppState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { curTime = Time.millisToPosix 0
      , timezone = Time.utc
      , state = Base
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = SawLightning
    | HeardThunder
    | Reset
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | curTime = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | timezone = newZone }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.timezone model.curTime)
                |> String.padLeft 2 '0'

        minute =
            String.fromInt (Time.toMinute model.timezone model.curTime)
                |> String.padLeft 2 '0'

        second =
            String.fromInt (Time.toSecond model.timezone model.curTime)
                |> String.padLeft 2 '0'

        time =
            hour ++ ":" ++ minute ++ ":" ++ second
    in
    div []
        [ div [] [ text ("Wait for the lightning: " ++ time) ]
        , button [ onClick SawLightning ] [ text "Lightning" ]
        , button [ onClick HeardThunder ] [ text "Thunder" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]
