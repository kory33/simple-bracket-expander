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
import Maybe.Extra exposing (join, unwrap)
import Format exposing (..)


---- MODEL ----


type alias Model =
    { input : String
    , output : ParserOutput
    , pendingUpdateCount : Int
    , config : ExpanderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = "", output = Ok "", pendingUpdateCount = 0, config = ExpanderConfig "  " 140 }, Cmd.none )



---- UPDATE ----


type ConfigUpdateParam
    = IndentString String
    | MaxRowLength String


type Msg
    = InputChange String
    | DelayedUpdate Int
    | ConfigUpdate ConfigUpdateParam


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
        ConfigUpdate updateParam ->
            let
                config = model.config
                newConfig =
                    case updateParam of
                        IndentString string -> { config | indentation = string }
                        MaxRowLength string ->
                            let
                                convertedInt = String.toInt string
                            in
                                case convertedInt of
                                    Just integer -> { config | maxFlatExpressionLength = integer }
                                    _ -> config
            in
                update (InputChange model.input) { model | config = newConfig }


---- VIEW ----


outputToString : ParserOutput -> String
outputToString output =
    let
        flattenedOutput =
            output
                |> Result.andThen (\string -> if string == "" then Err [] else Ok string)
                |> Result.toMaybe
    in
        unwrap "No output available" identity flattenedOutput


view : Model -> Html Msg
view model =
    let
        outputBlock : Html msg
        outputBlock = pre [ class "output-pre" ] [ code [] [ text <| outputToString model.output ] ]

        parseStatusHtml : Html msg
        parseStatusHtml =
            let
                boxColorClass : String
                boxColorClass =
                    case model.output of
                        Ok output ->
                            if output /= "" then " status-box-success"
                            else " status-box-empty"
                        Err _ -> " status-box-error"

                deadEndToString : DeadEnd -> String
                deadEndToString { row, col, problem } = "at row: " ++ (String.fromInt row) ++ ", col: " ++ (String.fromInt col)

                deadEndsToMessage : List DeadEnd -> String
                deadEndsToMessage deadEnds =
                    "Errors found\n" ++ (indentMultiline " " <| String.join "\n" <| List.map deadEndToString deadEnds)

                informationText : String
                informationText =
                    case model.output of
                        Ok output ->
                            if output /= "" then "Parsed and formatted successfully!"
                            else "Waiting for input..."
                        Err deadEnds -> deadEndsToMessage deadEnds
            in
                div [ class <| "box output-status-box" ++ boxColorClass]
                    [ div [ class "level" ] [ pre [] [ code [] [ text informationText ] ] ]
                    ]
        
        parameterForm : Html Msg
        parameterForm =
            div [ class "box" ]
                [ div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-normal" ]
                        [ label [ class "label" ] [ text "Indent characters" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ p [ class "control" ]
                                [ input
                                    [ class "input"
                                    , placeholder "  "
                                    , type_ "text"
                                    , onInput <| IndentString >> ConfigUpdate
                                    ] []
                                ]
                            ]
                        ]
                    ]
                , div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-normal" ]
                        [ label [ class "label" ] [ text "Line length" ] ]
                    , div [ class "field-body" ]
                        [ div [ class "field" ]
                            [ p [ class "control" ]
                                [ input
                                    [ class "input"
                                    , placeholder "140"
                                    , type_ "text"
                                    , onInput <| MaxRowLength >> ConfigUpdate
                                    ] []
                                ]
                            ]
                        ]
                    ]
                ]
    in
        div []
            [ nav [ class "navbar is-light" ]
                [ div [ class "navbar-brand" ]
                    [ div [ class "navbar-item container is-fluid" ]
                        [ h1 [ class "title" ] [ text "Simple bracket expander" ] ]
                    ]
                ]
            , div [ class "columns main-container" ]
                [ div [ class "column is-half"]
                    [ textarea [ class "textarea", placeholder "Input text", rows 14, onInput InputChange ] []
                    , parseStatusHtml
                    , parameterForm
                    ]
                , div [ class "column is-half" ] [ outputBlock ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
