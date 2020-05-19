module Main exposing (AddStep, Model, Msg(..), RemoveStep, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, value)
import Html.Events exposing (onClick, onInput)
import String exposing (length)
import Task
import Time


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Step =
    { name : String
    }


type alias Model =
    { time : Time.Posix
    , zone : Time.Zone
    , steps : List Step
    , stepInputContent : String
    }


type Msg
    = AddStep
    | RemoveStep
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ChangeStepInputContent String


type alias ChangeStepInputContent =
    String


type alias AddStep =
    Int


type alias RemoveStep =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0) Time.utc [] ""
    , Task.perform AdjustTimeZone Time.here
    )


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)
    in
    div []
        [ div [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , ul [] (List.map renderStep model.steps)
        , input [ placeholder "E.g. 'First prove'", value model.stepInputContent, onInput ChangeStepInputContent ] []
        , button [ onClick AddStep, disabled (length model.stepInputContent == 0) ] [ text "Add a new step" ]
        ]


renderStep : Step -> Html Msg
renderStep step =
    li [ class "step" ] [ text step.name ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddStep ->
            ( { model | steps = Step model.stepInputContent :: model.steps }, Cmd.none )

        RemoveStep ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        ChangeStepInputContent content ->
            ( { model | stepInputContent = content }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
