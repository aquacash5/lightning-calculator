module Main exposing (..)

import Browser exposing (Document)
import Html exposing (button, div, h1, p, text)
import Html.Events exposing (onClick)
import Round
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.document
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


view : Model -> Document Msg
view model =
    { title = "Lightning Distance Calculator"
    , body =
        [ case model.state of
            Base ->
                div []
                    [ h1 [] [ text "Wait for the lightning" ]
                    , button [ onClick SawLightning ] [ text "Lightning" ]
                    ]

            Lightning time ->
                div []
                    [ h1 [] [ text "Wait for the thunder" ]
                    , button [ onClick (HeardThunder time) ] [ text "Thunder" ]
                    , button [ onClick Reset ] [ text "Reset" ]
                    ]

            Thunder strike ->
                let
                    diff =
                        Time.posixToMillis strike.end - Time.posixToMillis strike.start

                    fDiff =
                        toFloat diff / 1000.0

                    distance =
                        fDiff / 5.0

                    roundedDistance =
                        Round.round 2 distance
                in
                div []
                    [ h1 [] [ text "Distance from the strike point" ]
                    , p [] [ text (roundedDistance ++ " miles") ]
                    , button [ onClick Reset ] [ text "Reset" ]
                    ]
        ]
    }


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
