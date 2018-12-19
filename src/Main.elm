module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Time exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task



---- MODEL ----


type alias Model =
    { input : String
    , output : String
    , lastInputTime : Maybe Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = "", output = "", lastInputTime = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = InputChange String
    | UpdateWithTime Posix


computeOutput : String -> String
computeOutput input = input


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChange inputString ->
            let
                updateTimeCmd = Task.perform UpdateWithTime Time.now
            in
                ( { model | input = inputString }, updateTimeCmd )
        UpdateWithTime posix ->
            let
                shouldRecomputeOutput =
                    case model.lastInputTime of
                        Just oldTime ->
                            let
                                oldTimePosix = posixToMillis oldTime
                                newTimePosix = posixToMillis posix
                            in
                                (newTimePosix - oldTimePosix) > 1000
                        _ -> True
                updatedModel =
                    if shouldRecomputeOutput
                        then
                            { model | output = computeOutput model.input }
                        else model
            in
                ( { updatedModel | lastInputTime = Just posix }, Cmd.none )



---- VIEW ----


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
                [ textarea [ class "textarea", placeholder "Output shown here", readonly True, value model.output ] [] ]
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
