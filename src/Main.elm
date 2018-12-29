port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, footer, h1, header, i, li, main_, p, section, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Task
import Time


port cache : E.Value -> Cmd msg



-- CUSTOM TYPES


type Emphasis
    = Focus
    | Task
    | Rest
    | Void
    | NoEmphasis


emphasisToString : Emphasis -> String
emphasisToString emphasis =
    case emphasis of
        Focus ->
            "Focus"

        Task ->
            "Task"

        Rest ->
            "Rest"

        Void ->
            "Void"

        NoEmphasis ->
            ""


stringToEmphasis : String -> Emphasis
stringToEmphasis emphasis =
    if emphasis == "Focus" then
        Focus

    else if emphasis == "Task" then
        Task

    else if emphasis == "Rest" then
        Rest

    else if emphasis == "Void" then
        Void

    else
        NoEmphasis



-- MODEL


type alias Model =
    { time : Time.Posix
    , emphasis : Emphasis
    , focus : Int
    , task : Int
    , rest : Int
    , void : Int
    }


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "time", E.int (Time.posixToMillis model.time) )
        , ( "emphasis", E.string (emphasisToString model.emphasis) )
        , ( "focus", E.int model.focus )
        , ( "task", E.int model.task )
        , ( "rest", E.int model.rest )
        , ( "void", E.int model.void )
        ]


decodeModel : D.Decoder Model
decodeModel =
    D.map6 Model
        decodeTime
        decodeEmphasis
        (D.field "focus" D.int)
        (D.field "task" D.int)
        (D.field "rest" D.int)
        (D.field "void" D.int)


decodeTime : D.Decoder Time.Posix
decodeTime =
    D.map Time.millisToPosix
        (D.field "time" D.int)


decodeEmphasis : D.Decoder Emphasis
decodeEmphasis =
    D.map stringToEmphasis
        (D.field "emphasis" D.string)



-- INIT


init : D.Value -> ( Model, Cmd Msg )
init flags =
    case D.decodeValue decodeModel flags of
        Ok cachedModel ->
            ( cachedModel
            , Cmd.none
            )

        Err error ->
            ( Model (Time.millisToPosix 0) NoEmphasis 0 0 0 0
            , Task.perform StartTime Time.now
            )



-- UPDATE


type Msg
    = StartTime Time.Posix
    | NewTime Time.Posix
    | ChangeEmphasis Emphasis


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartTime startTime ->
            let
                newModel =
                    { model | time = startTime }
            in
            ( newModel
            , cache (encodeModel newModel)
            )

        NewTime newTime ->
            let
                newModel =
                    if wasMidnight model.time newTime then
                        Model (lastMidnight newTime) model.emphasis 0 0 0 0

                    else
                        let
                            deltaMillis =
                                Time.posixToMillis newTime - Time.posixToMillis model.time
                        in
                        case model.emphasis of
                            NoEmphasis ->
                                { model | time = newTime }

                            Focus ->
                                { model | time = newTime, focus = model.focus + deltaMillis }

                            Task ->
                                { model | time = newTime, task = model.task + deltaMillis }

                            Rest ->
                                { model | time = newTime, rest = model.rest + deltaMillis }

                            Void ->
                                { model | time = newTime, void = model.void + deltaMillis }
            in
            ( newModel
            , cache (encodeModel newModel)
            )

        ChangeEmphasis newEmphasis ->
            let
                newModel =
                    { model | emphasis = newEmphasis }
            in
            ( newModel
            , cache (encodeModel newModel)
            )


posixDays : Time.Posix -> Int
posixDays time =
    let
        millisInDay =
            1000 * 60 * 60 * 24

        currentMillis =
            Time.posixToMillis time
    in
    currentMillis // millisInDay


wasMidnight : Time.Posix -> Time.Posix -> Bool
wasMidnight refTime time =
    let
        refDays =
            posixDays refTime

        days =
            posixDays time
    in
    days > refDays


lastMidnight : Time.Posix -> Time.Posix
lastMidnight time =
    let
        millisInDay =
            1000 * 60 * 60 * 24

        days =
            posixDays time

        millisInDays =
            days * millisInDay
    in
    Time.millisToPosix millisInDays



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 NewTime



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        titleText =
            emphasisToTitle model.emphasis
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


emphasisToTitle : Emphasis -> String
emphasisToTitle emphasis =
    let
        subtitle =
            emphasisToString emphasis
    in
    if subtitle /= "" then
        "Emphasis • " ++ subtitle

    else
        "Emphasis"


clock : Emphasis -> Int -> Html Msg
clock emphasis time =
    div [ class "clock" ]
        [ div [ class "clock-label" ] [ text (emphasisToString emphasis) ]
        , div [ class "clock-time" ]
            (let
                hours =
                    Time.toHour Time.utc (Time.millisToPosix time)

                minutes =
                    Time.toMinute Time.utc (Time.millisToPosix time)

                seconds =
                    Time.toSecond Time.utc (Time.millisToPosix time)
             in
             if hours > 0 then
                let
                    hoursText =
                        String.fromInt hours

                    decimal =
                        floor ((toFloat minutes / 60) * 10)
                in
                [ span []
                    [ text
                        (if decimal > 0 then
                            hoursText ++ "." ++ String.fromInt decimal

                         else
                            hoursText
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
            )
        ]


description : Emphasis -> List (Html Msg)
description emphasis =
    case emphasis of
        Focus ->
            [ p [] [ text "Important, meaningful, or time-sensitive work, requiring attention and effort." ]
            , ul []
                [ li [] [ text "An occupation" ]
                , li [] [ text "Studying for school" ]
                , li [] [ text "Paying overdue bills" ]
                ]
            ]

        Task ->
            [ p [] [ text "Though beneficial or necessary, these tasks easily interrupt more signficiant activity." ]
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
                , li [] [ text "Socializing, investing in relationships" ]
                , li [] [ text "Reflection, journaling" ]
                ]
            ]

        Void ->
            [ p [] [ text "Low-value activity that wastes time and resources, and may even cause harm." ]
            , ul []
                [ li [] [ text "Excessive social media usage" ]
                , li [] [ text "Binge entertainment consumption" ]
                , li [] [ text "Procrastination, idleness" ]
                ]
            ]

        NoEmphasis ->
            [ p [] [ text "A tool to foster mindfulness, intentionality, and balance by logging time." ]
            , p [] [ text "Tap on an ", i [] [ text "emphasis" ], text " below to begin." ]
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
                ChangeEmphasis NoEmphasis

             else
                ChangeEmphasis buttonEmphasis
            )
        ]
        [ div [ class "button-icon" ] []
        , div [ class "button-label" ] [ text (emphasisToString buttonEmphasis) ]
        ]



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
