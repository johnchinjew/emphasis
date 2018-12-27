module Main exposing (main)

import Browser
import Html exposing (Html, b, button, div, h1, i, li, p, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Task
import Time


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Emphasis
    = NoEmphasis
    | Focus
    | Task
    | Rest
    | Idle


type alias Model =
    { time : Time.Posix
    , focus : Int
    , task : Int
    , rest : Int
    , idle : Int
    , emphasis : Emphasis
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0) 0 0 0 0 NoEmphasis
    , Task.perform Reset Time.now
    )



-- UPDATE


type Msg
    = Reset Time.Posix
    | Tick Time.Posix
    | Emphasize Emphasis


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset startTime ->
            ( Model startTime 0 0 0 0 model.emphasis
            , Cmd.none
            )

        Tick newTime ->
            let
                deltaMillis =
                    Time.posixToMillis newTime - Time.posixToMillis model.time

                newModel =
                    case model.emphasis of
                        NoEmphasis ->
                            model

                        Focus ->
                            { model | focus = model.focus + deltaMillis }

                        Task ->
                            { model | task = model.task + deltaMillis }

                        Rest ->
                            { model | rest = model.rest + deltaMillis }

                        Idle ->
                            { model | idle = model.idle + deltaMillis }

                dayAsMillis =
                    24 * 60 * 60 * 1000
            in
            ( { newModel | time = newTime }
              -- , if Time.posixToMillis newTime >= dayAsMillis then -- FIXME
            , if False then
                Task.perform Reset Time.now

              else
                Cmd.none
            )

        Emphasize newEmphasis ->
            ( { model | emphasis = newEmphasis }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        titleText =
            title model.emphasis
    in
    { title = titleText
    , body =
        [ div [ class "app" ]
            [ h1 [ class "title" ] [ text titleText ]
            , div [ class "clocks" ]
                [ timer Focus model.focus
                , timer Task model.task
                , timer Rest model.rest
                , timer Idle model.idle
                ]
            , description model.emphasis
            , div [ class "buttons-outer" ]
                [ div [ class "buttons-inner" ]
                    [ btn Focus model.emphasis
                    , btn Task model.emphasis
                    , btn Rest model.emphasis
                    , btn Idle model.emphasis
                    ]
                ]
            ]
        ]
    }


emphasisToString : Emphasis -> String
emphasisToString emphasis =
    case emphasis of
        NoEmphasis ->
            ""

        Focus ->
            "Focus"

        Task ->
            "Task"

        Rest ->
            "Rest"

        Idle ->
            "Idle"


title : Emphasis -> String
title emphasis =
    let
        subtitle =
            emphasisToString emphasis
    in
    "Emphasis"
        ++ (if subtitle /= "" then
                " • " ++ subtitle

            else
                ""
           )


timer : Emphasis -> Int -> Html Msg
timer emphasis time =
    div [ class "clock" ]
        [ p [ class "clock-label" ] [ text (emphasisToString emphasis) ]
        , p [ class "clock-time" ] [ text (millisToString time) ]
        ]


description : Emphasis -> Html Msg
description emphasis =
    case emphasis of
        NoEmphasis ->
            p [] [ text "Tap on an ", i [] [ text "emphasis" ], text " below to begin." ]

        Focus ->
            p [] [ text "Important and time-sensitive tasks, requiring focus and effort." ]

        Task ->
            p [] [ text "Helpful but interruptive activity that may distract." ]

        Rest ->
            p [] [ text "Regenerative activity which yields long-term benefit." ]

        Idle ->
            p [] [ text "Low value activity that wastes time and resources and may even cause harm." ]


btn : Emphasis -> Emphasis -> Html Msg
btn buttonEmphasis modelEmphasis =
    let
        isOn =
            buttonEmphasis == modelEmphasis
    in
    button
        [ classList [ ( "button", True ), ( "button-on", isOn ) ]
        , onClick
            (if isOn then
                Emphasize Idle

             else
                Emphasize buttonEmphasis
            )
        ]
        [ div [ class "button-icon" ] []
        , div [ class "button-text" ] [ text (emphasisToString buttonEmphasis) ]
        ]


millisToString : Int -> String
millisToString time =
    let
        hours =
            Time.toHour Time.utc (Time.millisToPosix time)

        minutes =
            Time.toMinute Time.utc (Time.millisToPosix time)

        seconds =
            Time.toSecond Time.utc (Time.millisToPosix time)
    in
    if hours > 0 then
        let
            decimal =
                floor ((toFloat minutes / 60) * 10)
        in
        String.fromInt hours
            ++ (if decimal > 0 then
                    "." ++ String.fromInt decimal

                else
                    ""
               )
            ++ "h"

    else if minutes > 0 then
        String.fromInt minutes ++ "m"

    else
        String.fromInt seconds ++ "s"
