port module Main exposing (..)

import Html exposing (Html, Attribute, programWithFlags, div, text, h2, button, i, input, form, span)
import Html.Attributes exposing (class, value, style)
import Html.Events exposing (onClick, onSubmit, onInput)
import Time exposing (Time, second)


type Mode
    = Working
    | Resting


type alias Model =
    { secondsLeft : Int
    , running : Bool
    , mode : Mode
    , inputBox : String
    , todos : List String
    }


type alias Flag =
    { todos : List String }


type Msg
    = Tick Time
    | PlayPause
    | ToggleMode
    | ResetTime
    | InputBox String
    | AddTodo String
    | RemoveTodo Int


init : Flag -> ( Model, Cmd Msg )
init flag =
    ( Model (modeToSeconds Working) False Working "" flag.todos, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ pomodoroTimer model
        , viewTodos model
        ]


viewTodos : Model -> Html Msg
viewTodos model =
    div [ class "col-sm-4 col-sm-offset-4" ]
        [ form
            [ onSubmit (AddTodo model.inputBox) ]
            [ input
                [ class "form-control"
                , onInput InputBox
                , value model.inputBox
                ]
                []
            ]
        , div
            [ class "list-group text-center" ]
            (List.indexedMap viewTodo model.todos)
        ]


viewTodo : Int -> String -> Html Msg
viewTodo index todo =
    div [ class "list-group-item" ]
        [ text todo
        , div
            [ class "pull-right"
            , style [ ( "cursor", "pointer" ) ]
            ]
            [ span
                [ onClick (RemoveTodo index) ]
                [ viewGlyphicon "remove" ]
            ]
        ]


pomodoroTimer : Model -> Html Msg
pomodoroTimer model =
    div [ class "text-center" ]
        [ h2 []
            [ text (clockTime model.secondsLeft) ]
        , buttonGroup
            [ resetButton
            , playPauseButton model.running
            , nextButton
            ]
        ]


buttonGroup : List (Html Msg) -> Html Msg
buttonGroup buttons =
    div [ class "btn-group" ] buttons


resetButton : Html Msg
resetButton =
    viewButton
        [ onClick ResetTime ]
        [ viewGlyphicon "step-backward" ]


playPauseButton : Bool -> Html Msg
playPauseButton playing =
    let
        glyphiconText =
            if playing then
                "pause"
            else
                "play"
    in
        viewButton
            [ onClick PlayPause ]
            [ viewGlyphicon glyphiconText ]


nextButton : Html Msg
nextButton =
    viewButton
        [ onClick ToggleMode ]
        [ viewGlyphicon "step-forward" ]


viewGlyphicon : String -> Html Msg
viewGlyphicon glyphiconText =
    i [ class ("glyphicon glyphicon-" ++ glyphiconText) ] []


viewButton : List (Attribute Msg) -> List (Html Msg) -> Html Msg
viewButton attributes children =
    button
        ([ class "btn btn-default" ]
            ++ attributes
        )
        children


clockTime : Int -> String
clockTime totalSeconds =
    let
        seconds =
            String.padLeft 2 '0' (toString <| rem totalSeconds 60)

        minutes =
            toString <| rem (totalSeconds // 60) 60
    in
        minutes ++ ":" ++ seconds


toggleMode : Mode -> Mode
toggleMode mode =
    case mode of
        Working ->
            Resting

        Resting ->
            Working


modeToSeconds : Mode -> Int
modeToSeconds mode =
    case mode of
        Working ->
            25 * 60

        Resting ->
            5 * 60


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            if model.secondsLeft - 1 < 0 then
                ( { model
                    | secondsLeft = model.mode |> toggleMode |> modeToSeconds
                  }
                , makeSound ()
                )
            else
                ( { model | secondsLeft = model.secondsLeft - 1 }
                , updateTitle (clockTime (model.secondsLeft - 1))
                )

        PlayPause ->
            ( { model | running = not model.running }, Cmd.none )

        ToggleMode ->
            let
                mode =
                    toggleMode model.mode
            in
                ( { model | mode = mode, secondsLeft = modeToSeconds mode }
                , Cmd.none
                )

        ResetTime ->
            ( { model | secondsLeft = modeToSeconds model.mode }, Cmd.none )

        InputBox inputBox ->
            ( { model | inputBox = inputBox }, Cmd.none )

        AddTodo inputBox ->
            let
                todos =
                    model.todos ++ [ inputBox ]
            in
                ( { model | todos = todos, inputBox = "" }, saveTodos todos )

        RemoveTodo index ->
            let
                frontTodos =
                    List.take index model.todos

                backTodos =
                    List.drop (index + 1) model.todos

                todos =
                    frontTodos ++ backTodos
            in
                ( { model | todos = todos }, saveTodos todos )


port saveTodos : List String -> Cmd msg


port updateTitle : String -> Cmd msg


port makeSound : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every second Tick
    else
        Sub.none


main : Program Flag Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
