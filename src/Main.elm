module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, p, text)
import Html.Events exposing (onClick)
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }



-- MODEL


type AppState
    = Base
    | Lightning Time.Posix
    | Thunder { start : Time.Posix, end : Time.Posix }


type alias Model =
    { timezone : Time.Zone
    , state : AppState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { timezone = Time.utc
      , state = Base
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = AdjustTimeZone Time.Zone
    | SawLightning
    | CaptureLightning Time.Posix
    | HeardThunder Time.Posix
    | CaptureThunder Time.Posix Time.Posix
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | timezone = newZone }
            , Cmd.none
            )

        SawLightning ->
            ( model
            , Task.perform CaptureLightning Time.now
            )

        CaptureLightning curTime ->
            ( { model | state = Lightning curTime }
            , Cmd.none
            )

        HeardThunder lightning ->
            ( model
            , Task.perform (CaptureThunder lightning) Time.now
            )

        CaptureThunder lightning curTime ->
            ( { model
                | state =
                    Thunder
                        { start = lightning
                        , end = curTime
                        }
              }
            , Cmd.none
            )

        Reset ->
            ( { model | state = Base }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Base ->
            div []
                [ h1 [] [ text "Wait for the lightning" ]
                , button [ onClick SawLightning ] [ text "Lightning" ]
                ]

        Lightning time ->
            div []
                [ h1 [] [ text "Wait for the thunder" ]
                , p [] [ text (toTimeString model.timezone time) ]
                , button [ onClick (HeardThunder time) ] [ text "Thunder" ]
                , button [ onClick Reset ] [ text "Reset" ]
                ]

        Thunder strike ->
            div []
                [ h1 [] [ text "Distance from the strike point" ]
                , p [] [ text (toTimeString model.timezone strike.start) ]
                , p [] [ text (toTimeString model.timezone strike.end) ]
                , button [ onClick Reset ] [ text "Reset" ]
                ]


toTimeString : Time.Zone -> Time.Posix -> String
toTimeString zone posix =
    let
        hour =
            String.fromInt (Time.toHour zone posix) |> String.padLeft 2 '0'

        minute =
            String.fromInt (Time.toMinute zone posix) |> String.padLeft 2 '0'

        second =
            String.fromInt (Time.toSecond zone posix) |> String.padLeft 2 '0'

        millis =
            String.fromInt (Time.toMillis zone posix) |> String.padLeft 3 '0'
    in
    hour ++ ":" ++ minute ++ ":" ++ second ++ ":" ++ millis
