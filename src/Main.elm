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



-- EMPHASIS


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



-- TIME


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


wasMidnight : Time.Zone -> Time.Posix -> Time.Posix -> Bool
wasMidnight zone refTime time =
    let
        refDay =
            Time.toDay zone refTime

        refMonth =
            monthToInt (Time.toMonth zone refTime)

        refYear =
            Time.toYear zone refTime

        day =
            Time.toDay zone time

        month =
            monthToInt (Time.toMonth zone time)

        year =
            Time.toYear zone time
    in
    if year > refYear then
        True

    else if month > refMonth then
        True

    else if day > refDay then
        True

    else
        False


isMidnight : Time.Zone -> Time.Posix -> Bool
isMidnight zone time =
    let
        hours =
            Time.toHour zone time

        minutes =
            Time.toMinute zone time

        seconds =
            Time.toSecond zone time
    in
    hours == 0 && minutes == 0 && seconds == 0


prevSecond : Time.Posix -> Time.Posix
prevSecond time =
    Time.millisToPosix (Time.posixToMillis time - 1000)


lastMidnight : Time.Zone -> Time.Posix -> Time.Posix
lastMidnight zone time =
    if isMidnight zone time then
        time

    else
        lastMidnight zone (prevSecond time)



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , emphasis : Emphasis
    , focus : Int
    , task : Int
    , rest : Int
    , void : Int
    }


encodeModel : Model -> E.Value
encodeModel model =
    -- NOTE: We do not encode the model.zone
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
    -- NOTE: We reset the model.zone here
    D.map6 (Model Time.utc)
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
            , Task.perform AdjustTimeZone Time.here
            )

        Err error ->
            ( Model Time.utc (Time.millisToPosix 0) NoEmphasis 0 0 0 0
            , Cmd.batch
                [ Task.perform AdjustTimeZone Time.here
                , Task.perform StartTime Time.now
                ]
            )



-- UPDATE


type Msg
    = StartTime Time.Posix
    | AdjustTimeZone Time.Zone
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

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        NewTime newTime ->
            let
                newModel =
                    if wasMidnight model.zone model.time newTime then
                        Model model.zone (lastMidnight model.zone newTime) model.emphasis 0 0 0 0

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
            , Cmd.batch
                [ Task.perform AdjustTimeZone Time.here
                , cache (encodeModel newModel)
                ]
            )

        ChangeEmphasis newEmphasis ->
            let
                newModel =
                    { model | emphasis = newEmphasis }
            in
            ( newModel
            , cache (encodeModel newModel)
            )



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
                , li [] [ text "Following the news" ]
                ]
            ]

        Rest ->
            [ p [] [ text "Regenerative activity which yields long-term benefit, but is often neglected." ]
            , ul []
                [ li [] [ text "Rest, play, exercise, hobbies, meals" ]
                , li [] [ text "Socializing, investing in relationships" ]
                , li [] [ text "Reflection, journaling, reading" ]
                ]
            ]

        Void ->
            [ p [] [ text "Low-value activity that wastes time and resources, and may even cause harm." ]
            , ul []
                [ li [] [ text "Excessive media consumption" ]
                , li [] [ text "Procrastination, idleness" ]
                , li [] [ text "Unbalanced overworking" ]
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
