module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task
import Process
import Expander exposing (computeOutput)
import CommonModel exposing (..)
import Parser exposing (DeadEnd)


---- MODEL ----


type alias Model =
    { input : String
    , output : Result (List DeadEnd) String
    , pendingUpdateCount : Int
    , config : ExpanderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = "", output = Ok "", pendingUpdateCount = 0, config = ExpanderConfig "  " 20 }, Cmd.none )



---- UPDATE ----


type Msg
    = InputChange String
    | DelayedUpdate Int


-- delay the message invocation by specified miliseconds
delay : Msg -> Float -> Cmd Msg
delay msg miliseconds = Process.sleep miliseconds |> (Task.perform <| \_ -> msg)


updateDelay : Float
updateDelay = 150


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChange inputString ->
            let
                inputUpdatedModel = { model | input = inputString }
                newUpdateCount = model.pendingUpdateCount + 1
                countUpdatedModel = { inputUpdatedModel | pendingUpdateCount = newUpdateCount }
            in
                ( countUpdatedModel, delay (DelayedUpdate newUpdateCount) updateDelay )
        DelayedUpdate oldPendingUpdateCount ->
            if model.pendingUpdateCount == oldPendingUpdateCount then
                ( { model | output = computeOutput model.config model.input, pendingUpdateCount = 0 }, Cmd.none )
            else
                ( model, Cmd.none )


---- VIEW ----


outputToString : Result (List DeadEnd) String -> String
outputToString output =
    let
        flattenedOutput = output |> Result.andThen (\string -> if string == "" then Err [] else Ok string)
    in
        case flattenedOutput of
            Ok s -> s
            Err _ -> "No output available."


view : Model -> Html Msg
view model =
    div []
        [ nav [ class "navbar is-light" ]
            [ div [ class "navbar-brand" ]
                [ div [ class "navbar-item container is-fluid" ]
                    [ h1 [ class "title" ] [ text "Simple bracket expander" ] ]
                ]
            ]
        , div [ class "columns main-container" ]
            [ div [ class "column "]
                [ textarea [ class "textarea", placeholder "Input text", rows 14, onInput InputChange ] [] ]
            , div [ class "column" ]
                [ pre []
                    [ code []
                        [ text <| outputToString model.output ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }