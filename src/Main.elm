module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, h1, img, p, text)
import Html.Attributes exposing (alt, class, href, src, style, target, title)
import Html.Events exposing (onClick)
import Round
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
    div [ class "container" ]
        [ div [ class "text-center" ] [ logo, viewState model ]
        , div [ class "fixed-bottom m-2" ]
            [ div [ class "row" ]
                [ div [ class "col order-md-last" ]
                    [ div [ class "float-md-end" ]
                        [ text "Lightning Distance Calculator by "
                        , a
                            [ href "https://github.com/aquacash5/"
                            , target "_blank"
                            , title "Github"
                            ]
                            [ text "Kyle Bloom" ]
                        ]
                    ]
                , div [ class "w-100 d-md-none d-block" ] []
                , div [ class "col" ]
                    [ text "Lightning by b a r z i n from "
                    , a
                        [ href "https://thenounproject.com/icon/lightning-759722/"
                        , target "_blank"
                        , title "Lightning Icons"
                        ]
                        [ text "Noun Project" ]
                    ]
                ]
            ]
        ]


logo : Html Msg
logo =
    img
        [ src "images/lightning.png"
        , alt "Lightning"
        , style "max-height" "300px"
        , style "max-width" "300px"
        ]
        []


viewState : Model -> Html Msg
viewState model =
    div [ class "vstack gap-3 col-md-6 mx-auto" ]
        (case model.state of
            Base ->
                [ h1 [] [ text "Wait for the lightning" ]
                , button
                    [ class "btn btn-primary btn-lg"
                    , onClick SawLightning
                    ]
                    [ text "Lightning" ]
                ]

            Lightning time ->
                [ h1 [] [ text "Wait for the thunder" ]
                , button
                    [ class "btn btn-primary btn-lg"
                    , onClick (HeardThunder time)
                    ]
                    [ text "Thunder" ]
                , button
                    [ class "btn btn-outline-danger btn-lg"
                    , onClick Reset
                    ]
                    [ text "Reset" ]
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
                [ h1 [] [ text "Distance from the lightning" ]
                , button
                    [ class "btn btn-outline-danger btn-lg"
                    , onClick Reset
                    ]
                    [ text "Reset" ]
                , p [ class "" ] [ text (roundedDistance ++ " miles") ]
                , p [ class "" ] [ text (String.fromFloat fDiff ++ " seconds") ]
                ]
        )


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
