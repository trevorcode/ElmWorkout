port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, label, main_, pre, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Http
import Task
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Workout
    = Workout
    | Cooldown
    | Stopped


type alias WorkoutState =
    { time : Float
    , cooldownTime : Float
    , countdownTime : Float
    , workout : LoadedWorkout
    , workoutState : Workout
    , workoutId : Int
    }


type LoadedWorkout
    = Failure
    | Loading
    | Success String


type alias Model =
    WorkoutState


init : () -> ( Model, Cmd Msg )
init _ =
    ( WorkoutState 30 20 30 Loading Stopped 1
    , Http.get
        { url = "workouts/" ++ String.fromInt 1 ++ ".txt"
        , expect = Http.expectString <| GotText 1
        }
    )


port playAudio : String -> Cmd msg


type NextOrPrev
    = Next
    | Prev


type Msg
    = GotText Int (Result Http.Error String)
    | Tick Time.Posix
    | ToggleWorkout
    | LoadWorkout NextOrPrev
    | UpdateTimers String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadWorkout x ->
            case x of
                Next ->
                    ( model
                    , Http.get
                        { url = "workouts/" ++ String.fromInt (model.workoutId + 1) ++ ".txt"
                        , expect = Http.expectString <| GotText (model.workoutId + 1)
                        }
                    )

                Prev ->
                    ( model
                    , Http.get
                        { url = "workouts/" ++ String.fromInt (model.workoutId - 1) ++ ".txt"
                        , expect = Http.expectString <| GotText (model.workoutId - 1)
                        }
                    )

        GotText id result ->
            case result of
                Ok fullText ->
                    ( { model | workout = Success fullText, workoutId = id }
                    , Cmd.none
                    )

                Err _ ->
                    if model.workout == Loading then
                        ( { model | workout = Failure }, Cmd.none )

                    else
                        ( model, Cmd.none )

        Tick time ->
            case model.workoutState of
                Workout ->
                    if model.time > 0 then
                        ( { model | time = model.time - 1 }, Cmd.none )

                    else
                        ( { model | time = model.cooldownTime, workoutState = Cooldown }, playAudio "stop" )

                Cooldown ->
                    if model.time > 0 then
                        ( { model | time = model.time - 1 }, Cmd.none )

                    else
                        ( { model | time = model.countdownTime, workoutState = Workout }, playAudio "start" )

                Stopped ->
                    ( model, Cmd.none )

        ToggleWorkout ->
            case model.workoutState of
                Stopped ->
                    ( { model | workoutState = Workout, time = model.countdownTime }, playAudio "start" )

                _ ->
                    ( { model | workoutState = Stopped, time = model.countdownTime }, Cmd.none )

        UpdateTimers timer amount ->
            let
                amountFloat =
                    String.toFloat amount |> Maybe.withDefault 0
            in
            case timer of
                "countDown" ->
                    if model.workoutState == Stopped then
                        ( { model | countdownTime = amountFloat, time = amountFloat }, Cmd.none )

                    else
                        ( { model | countdownTime = amountFloat }, Cmd.none )

                "coolDown" ->
                    ( { model | cooldownTime = amountFloat }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.workoutState /= Stopped then
        Time.every 1000 Tick

    else
        Sub.none


view : Model -> Html Msg
view model =
    main_ []
        [ label []
            [ text "Workout"
            , input [ onInput <| UpdateTimers "countDown", value <| String.fromFloat model.countdownTime ] []
            ]
        , label []
            [ text "Cooldown"
            , input [ onInput <| UpdateTimers "coolDown", value <| String.fromFloat model.cooldownTime ] []
            ]
        , div [ class "timer-container" ]
            [ button [ onClick ToggleWorkout, class "timer" ] [ text <| String.fromFloat model.time ]
            ]
        , case model.workout of
            Failure ->
                text "I was unable to load your workout."

            Loading ->
                text "Loading..."

            Success fullText ->
                div []
                    [ h1 [] [ text <| "Workout #" ++ String.fromInt model.workoutId ]
                    , div [ class "workoutText" ] [ text fullText ]
                    ]
        , button [ onClick <| LoadWorkout Prev ] [ text "Prev" ]
        , button [ onClick <| LoadWorkout Next ] [ text "Next" ]
        ]
