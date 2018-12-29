module Main exposing (main)

import Browser
import Html exposing (..)
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
    | Void


type alias Model =
    { time : Time.Posix
    , emphasis : Emphasis
    , focus : Int
    , task : Int
    , rest : Int
    , void : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0) NoEmphasis 0 0 0 0
    , Task.perform Reset Time.now
    )



-- UPDATE


type Msg
    = Reset Time.Posix
    | Tick Time.Posix
    | Select Emphasis


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset startTime ->
            ( Model startTime model.emphasis 0 0 0 0
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

                        Void ->
                            { model | void = model.void + deltaMillis }

                dayAsMillis =
                    24 * 60 * 60 * 1000
            in
            ( { newModel | time = newTime }
            , if posixDays newTime > posixDays model.time then
                Task.perform Reset Time.now

              else
                Cmd.none
            )

        Select newEmphasis ->
            ( { model | emphasis = newEmphasis }
            , Cmd.none
            )


posixDays : Time.Posix -> Int
posixDays time =
    let
        millisInDay =
            86400000
    in
    Time.posixToMillis time // millisInDay



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
        [ main_ [ class "app" ]
            [ header [ class "title" ]
                [ h1 [] [ text titleText ] ]
            , section [ class "clocks" ]
                [ clock Focus model.focus
                , clock Task model.task
                , clock Rest model.rest
                , clock Void model.void
                ]
            , section [ class "description" ] (description model.emphasis)
            , footer [ class "buttons-outer" ]
                [ div [ class "buttons-inner" ]
                    [ button_ Focus model.emphasis
                    , button_ Task model.emphasis
                    , button_ Rest model.emphasis
                    , button_ Void model.emphasis
                    ]
                ]
            ]
        ]
    }


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


clock : Emphasis -> Int -> Html Msg
clock emphasis time =
    div [ class "clock" ]
        [ div [ class "clock-label" ] [ text (emphasisToString emphasis) ]
        , div [ class "clock-time" ] (clockTime time)
        ]


clockTime : Int -> List (Html Msg)
clockTime time =
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
        [ span []
            [ text
                (String.fromInt hours
                    ++ (if decimal > 0 then
                            "." ++ String.fromInt decimal

                        else
                            ""
                       )
                )
            ]
        , span [ class "clock-unit" ] [ text "h" ]
        ]

    else if minutes > 0 then
        [ span [] [ text (String.fromInt minutes) ]
        , span [ class "clock-unit" ] [ text "m" ]
        ]

    else
        [ span [] [ text (String.fromInt seconds) ]
        , span [ class "clock-unit" ] [ text "s" ]
        ]


description : Emphasis -> List (Html Msg)
description emphasis =
    case emphasis of
        NoEmphasis ->
            [ p [] [ text "Foster mindfulness, intentionality, and balance by logging time." ]
            , p [] [ text "Tap on an ", i [] [ text "emphasis" ], text " below to begin." ]
            ]

        Focus ->
            [ p [] [ text "Important, meaningful, or time-sensitive work, requiring attention and effort." ]
            , ul []
                [ li [] [ text "An occupation" ]
                , li [] [ text "Studying for school" ]
                , li [] [ text "Paying overdue bills" ]
                ]
            ]

        Task ->
            [ p [] [ text "Though beneficial or necessary, tasks may interrupt more signficiant activity." ]
            , ul []
                [ li [] [ text "Reviewing emails" ]
                , li [] [ text "Reorganizing a desk" ]
                , li [] [ text "Reading the news" ]
                ]
            ]

        Rest ->
            [ p [] [ text "Regenerative activity which yields long-term benefit, but is often neglected." ]
            , ul []
                [ li [] [ text "Rest, play, exercise, hobbies, meals" ]
                , li [] [ text "Socializing and investing in relationships" ]
                , li [] [ text "Reflection and journaling" ]
                ]
            ]

        Void ->
            [ p [] [ text "Low-value activity that wastes time and resources, and may even cause harm." ]
            , ul []
                [ li [] [ text "Excessive social media usage" ]
                , li [] [ text "Binge entertainment consumption" ]
                , li [] [ text "Procrastination and idleness" ]
                ]
            ]


button_ : Emphasis -> Emphasis -> Html Msg
button_ buttonEmphasis modelEmphasis =
    let
        isOn =
            buttonEmphasis == modelEmphasis
    in
    button
        [ classList [ ( "button", True ), ( "button-on", isOn ) ]
        , onClick
            (if isOn then
                Select NoEmphasis

             else
                Select buttonEmphasis
            )
        ]
        [ div [ class "button-icon" ] []
        , div [ class "button-label" ] [ text (emphasisToString buttonEmphasis) ]
        ]


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

        Void ->
            "Void"
