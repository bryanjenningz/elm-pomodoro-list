port module Main exposing (..)

import Html exposing (Html, Attribute, programWithFlags, div, text, h2, button, i, input, form, span)
import Html.Attributes exposing (class, value, style, id)
import Html.Events exposing (onClick, onSubmit, onInput, onDoubleClick, onBlur)
import Time exposing (Time, second)
import Dom
import Task


type Mode
    = Working
    | Resting


type alias Model =
    { secondsLeft : Int
    , clockRunning : Bool
    , mode : Mode
    , inputBox : String
    , todos : List String
    , editIndex : Int
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
    | EditTodo Int String
    | NoOp


init : Flag -> ( Model, Cmd Msg )
init flag =
    ( Model (modeToSeconds Working) False Working "" flag.todos -1
    , Cmd.none
    )


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
            (List.indexedMap (viewTodo model.editIndex) model.todos)
        ]


viewTodo : Int -> Int -> String -> Html Msg
viewTodo editIndex index todo =
    div [ class "list-group-item" ]
        (if editIndex == index then
            [ form [ onSubmit (EditTodo -1 "") ]
                [ input
                    [ onInput (EditTodo index)
                    , onBlur (EditTodo -1 "")
                    , class "form-control"
                    , id "edit-input"
                    , value todo
                    ]
                    []
                ]
            ]
         else
            [ span [ onDoubleClick (EditTodo index todo) ]
                [ text todo ]
            , div
                [ class "pull-right"
                , style [ ( "cursor", "pointer" ) ]
                ]
                [ span
                    [ onClick (RemoveTodo index) ]
                    [ viewGlyphicon "remove" ]
                ]
            ]
        )


pomodoroTimer : Model -> Html Msg
pomodoroTimer model =
    div [ class "text-center" ]
        [ h2 []
            [ text (clockTime model.secondsLeft) ]
        , buttonGroup
            [ resetButton
            , playPauseButton model.clockRunning
            , nextButton
            ]
        ]


buttonGroup : List (Html Msg) -> Html Msg
buttonGroup buttons =
    div [ class "btn-group" ] buttons


resetButton : Html Msg
resetButton =
    viewButton [ onClick ResetTime ] [ viewGlyphicon "step-backward" ]


playPauseButton : Bool -> Html Msg
playPauseButton playing =
    let
        glyphiconText =
            if playing then
                "pause"
            else
                "play"
    in
        viewButton [ onClick PlayPause ] [ viewGlyphicon glyphiconText ]


nextButton : Html Msg
nextButton =
    viewButton [ onClick ToggleMode ] [ viewGlyphicon "step-forward" ]


viewGlyphicon : String -> Html Msg
viewGlyphicon glyphiconText =
    i [ class ("glyphicon glyphicon-" ++ glyphiconText) ] []


viewButton : List (Attribute Msg) -> List (Html Msg) -> Html Msg
viewButton attributes children =
    button ([ class "btn btn-default" ] ++ attributes) children


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
                    | secondsLeft = toggleMode model.mode |> modeToSeconds
                    , mode = toggleMode model.mode
                  }
                , makeSound ()
                )
            else
                ( { model | secondsLeft = model.secondsLeft - 1 }
                , updateTitle (clockTime (model.secondsLeft - 1))
                )

        PlayPause ->
            ( { model | clockRunning = not model.clockRunning }, Cmd.none )

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

        EditTodo index editText ->
            let
                todos =
                    List.indexedMap
                        (\i todo ->
                            if i == index then
                                editText
                            else
                                todo
                        )
                        model.todos
            in
                ( { model | editIndex = index, todos = todos }
                , Cmd.batch
                    [ saveTodos todos
                    , Task.attempt (\_ -> NoOp) (Dom.focus "edit-input")
                    ]
                )

        NoOp ->
            ( model, Cmd.none )


port saveTodos : List String -> Cmd msg


port updateTitle : String -> Cmd msg


port makeSound : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.clockRunning then
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
