module Main exposing (main)

import Browser
import Html exposing (Html, b, button, div, h1, i, li, p, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events
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
    = Idle
    | Critical
    | Meaningful
    | Interruptive
    | Subtractive


type alias Model =
    { time : Time.Posix
    , critical : Int
    , meaningful : Int
    , interruptive : Int
    , subtractive : Int
    , emphasis : Emphasis
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0) 0 0 0 0 Idle
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
                        Idle ->
                            model

                        Critical ->
                            { model | critical = model.critical + deltaMillis }

                        Meaningful ->
                            { model | meaningful = model.meaningful + deltaMillis }

                        Interruptive ->
                            { model | interruptive = model.interruptive + deltaMillis }

                        Subtractive ->
                            { model | subtractive = model.subtractive + deltaMillis }

                dayAsMillis =
                    24 * 60 * 60 * 1000
            in
            ( { newModel | time = newTime }
            , if Time.posixToMillis newTime >= dayAsMillis then
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
            , description model.emphasis
            , div [ class "clocks-outer" ]
                [ div [ class "clocks-inner" ]
                    [ clock Critical model.critical model.emphasis
                    , clock Meaningful model.meaningful model.emphasis
                    , clock Interruptive model.interruptive model.emphasis
                    , clock Subtractive model.subtractive model.emphasis
                    ]
                ]
            ]
        ]
    }


title : Emphasis -> String
title emphasis =
    let
        subtitle =
            case emphasis of
                Idle ->
                    ""

                Critical ->
                    "Critical"

                Meaningful ->
                    "Meaningful"

                Interruptive ->
                    "Interruptive"

                Subtractive ->
                    "Subtractive"
    in
    "Emphasis"
        ++ (if subtitle /= "" then
                " • " ++ subtitle

            else
                ""
           )


description : Emphasis -> Html Msg
description emphasis =
    let
        rest : List (Html Msg)
        rest =
            case emphasis of
                Idle ->
                    [ p [] [ text "This is ", b [] [ text "Emphasis" ], text ", a tool to help you live more intentionally by:" ]
                    , ul []
                        [ li [] [ text "showing you how you spend time" ]
                        , li [] [ text "pushing you to fully engage the moment" ]
                        , li [] [ text "encouraging you to live with balance" ]
                        ]
                    , p [] [ text "Every thing you do (from having lunch to writing a poem) falls within an ", i [] [ text "emphasis" ], text ". Tap on any emphasis below to begin." ]
                    ]

                Critical ->
                    [ p [] [ text "You're working on something ", b [] [ text "Critical" ], text ": important and time-sensitive work, requiring focus and effort. Keep going!" ]
                    , p [] [ text "Examples:" ]
                    , ul []
                        [ li [] [ text "Your day job" ]
                        , li [] [ text "Homework" ]
                        , li [] [ text "Paying overdue bills" ]
                        ]
                    ]

                Meaningful ->
                    [ p [] [ text "You're doing something ", b [] [ text "Meaningful" ], text ": intrinsically valuable activity which yields long-term benefit for yourself and others, but is often regretfully neglected." ]
                    , p [] [ text "Examples:" ]
                    , ul []
                        [ li [] [ text "Rest, play, exercise, meals" ]
                        , li [] [ text "Investing in relationships" ]
                        , li [] [ text "Journaling and reflection" ]
                        , li [] [ text "Enjoying hobbies and pursuing goals" ]
                        ]
                    ]

                Interruptive ->
                    [ p [] [ text "You're completing ", b [] [ text "Interruptive" ], text " tasks: somewhat helpful or necessary, but they can easily distract you from more important things. Try to finish these all at once in a batch." ]
                    , p [] [ text "Examples:" ]
                    , ul []
                        [ li [] [ text "Reading emails" ]
                        , li [] [ text "Checking notifications" ]
                        , li [] [ text "Reorganizing desk" ]
                        ]
                    ]

                Subtractive ->
                    [ p [] [ text "Be careful, you're doing something ", b [] [ text "Subtractive" ], text ": low value activity that wastes time and resources and may even cause harm. These are things you'd like to do less or stop doing entirely." ]
                    , p [] [ text "Examples:" ]
                    , ul []
                        [ li [] [ text "Excessive entertainment consumption" ]
                        , li [] [ text "Excessive social media usage" ]
                        , li [] [ text "Periods of idleness" ]
                        ]
                    ]
    in
    div [ class "description" ] rest


clock : Emphasis -> Int -> Emphasis -> Html Msg
clock clockEmphasis time currentEmphasis =
    let
        isOn =
            clockEmphasis == currentEmphasis
    in
    button
        [ classList [ ( "clock", True ), ( "clock-on", isOn ) ]
        , Html.Events.onClick
            (if isOn then
                Emphasize Idle

             else
                Emphasize clockEmphasis
            )
        ]
        [ div [ class "clock-icon" ] []
        , div [ class "clock-time" ] [ text (millisToString time) ]
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
