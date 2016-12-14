port module Main exposing (..)

import Html exposing (Html, Attribute, programWithFlags, div, text, h2, button, i, input, form, span)
import Html.Attributes exposing (class, value, style)
import Html.Events exposing (onClick, onSubmit, onInput)
import Time exposing (Time, second)


init : Model -> (Model, Cmd Msg)
init model =
    ( model, Cmd.none )


type alias Model =
    { time : Int -- Time in seconds
    , running : Bool -- Is the clock running?
    , sprinting : Bool -- Is it sprinting mode or resting mode? (25 minutes or 5 minutes?)
    , text : String
    , todos : List String
    }


type Msg
    = Tick Time
    | PlayPause
    | ToggleSprinting
    | ResetTime
    | ChangeText String
    | AddTodo String
    | RemoveTodo Int


view : Model -> Html Msg
view model =
    div []
        [ pomodoroTimer model
        , viewTodos model
        ]


viewTodos : Model -> Html Msg
viewTodos model =
    div [ class "col-sm-4 col-sm-offset-4"]
        [ form
            [ onSubmit (AddTodo model.text) ]
            [ input
                [ class "form-control"
                , onInput ChangeText
                , value model.text
                ]
                []
            ]
        , div
            [ class "list-group text-center" ]
            ( List.indexedMap viewTodo model.todos )
        ]


viewTodo : Int -> String -> Html Msg
viewTodo index todo =
    div [ class "list-group-item" ]
        [ text todo
        , div
            [ class "pull-right"
            , style [ ("cursor", "pointer") ]
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
            [ text (clockTime model.time) ]
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
            if playing then "pause" else "play"
    in
        viewButton
            [ onClick PlayPause ]
            [ viewGlyphicon glyphiconText ]


nextButton : Html Msg
nextButton =
    viewButton
        [ onClick ToggleSprinting ]
        [ viewGlyphicon "step-forward" ]


viewGlyphicon : String -> Html Msg
viewGlyphicon glyphiconText =
    i [ class ("glyphicon glyphicon-" ++ glyphiconText) ] []


viewButton : List (Attribute Msg) -> List (Html Msg) -> Html Msg
viewButton attributes children =
    button
        ( [ class "btn btn-default" ]
            ++ attributes
        )
        children


clockTime : Int -> String
clockTime totalSeconds =
    let
        seconds =
            String.right 2 <| "0" ++ (toString <| rem totalSeconds 60)
        minutes =
            toString <| rem (totalSeconds // 60) 60
    in
        minutes ++ ":" ++ seconds


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            let
                timeChange =
                    if model.running then 1 else 0
                decrementedTime =
                    model.time - timeChange
                ( time, shouldMakeSound ) =
                    if decrementedTime < 0 then
                        ( if model.sprinting then 5*60 else 25*60, True )
                    else
                        ( decrementedTime, False )
            in
                saveModel
                    ( { model | time = time }
                    , if shouldMakeSound then sound 1 else Cmd.none
                    )

        PlayPause ->
            saveModel ( { model | running = not model.running }, Cmd.none )

        ToggleSprinting ->
            let
                sprinting =
                    not model.sprinting
            in
                saveModel
                    ( { model
                      | sprinting = sprinting
                      , time = if sprinting then (25*60) else (5*60)
                      }
                    , Cmd.none
                    )

        ResetTime ->
            let
                time =
                    if model.sprinting then (25*60) else (5*60)
            in
                saveModel ( { model | time = time }, Cmd.none )

        ChangeText text ->
            saveModel ( { model | text = text }, Cmd.none )

        AddTodo text ->
            saveModel
                ( { model
                  | todos = model.todos ++ [ text ]
                  , text = ""
                  }
                , Cmd.none
                )

        RemoveTodo index ->
            let
                frontTodos =
                    List.take index model.todos
                backTodos =
                    List.drop (index + 1) model.todos
                todos =
                    frontTodos ++ backTodos
            in
                saveModel ( { model | todos = todos }, Cmd.none )


saveModel : (Model, Cmd Msg) -> (Model, Cmd Msg)
saveModel tuple =
    let
        model =
            Tuple.first tuple
        cmd =
            Tuple.second tuple
    in
        ( model, Cmd.batch [ cmd, save model ] )


port save : Model -> Cmd msg
port sound : Int -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


main : Program Model Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
