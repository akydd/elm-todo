module Main exposing (Model, Msg(..), Todo, init, main, update)

import Browser
import Html exposing (Attribute, Html, button, div, input, li, s, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
    { description : String
    , complete : Bool
    , id : Int
    }


type alias Model =
    { todos : List Todo
    , newId : Int
    , newDescription : String
    , filter : String
    }


init : Model
init =
    { todos = []
    , newId = 0
    , newDescription = ""
    , filter = "All"
    }


newTodo : Int -> String -> Todo
newTodo newId newDescription =
    Todo newDescription False newId



-- UPDATE


type Msg
    = AddTodo
    | ToggleTodoComplete Int
    | Change String
    | Filter String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            { model | newId = model.newId + 1, todos = newTodo model.newId model.newDescription :: model.todos, newDescription = "" }

        ToggleTodoComplete id ->
            let
                toggleTodo todo =
                    if todo.id == id then
                        { todo | complete = not todo.complete }

                    else
                        todo

                updatedTodos =
                    List.map toggleTodo model.todos
            in
            { model | todos = updatedTodos }

        Change a ->
            { model | newDescription = a }

        Filter filterType ->
            { model | filter = filterType }



-- VIEW


viewTodo : Todo -> Html Msg
viewTodo todo =
    let
        formatTodo t =
            if t.complete then
                s [] [ text t.description ]

            else
                text t.description
    in
    li [ onClick (ToggleTodoComplete todo.id) ] [ formatTodo todo ]


viewTodos : List Todo -> String -> Html Msg
viewTodos todos filterType =
    case filterType of
        "All" ->
            ul [] (List.map viewTodo todos)

        "Active" ->
            ul [] (List.map viewTodo (List.filter (\t -> not t.complete) todos))

        "Completed" ->
            ul [] (List.map viewTodo (List.filter (\t -> t.complete) todos))

        _ ->
            ul [] []


viewFilter : String -> String -> Html Msg
viewFilter filter activeFilter =
    button [ onClick (Filter filter), disabled (filter == activeFilter) ] [ text filter ]


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Enter description", value model.newDescription, onInput Change ] []
        , button [ onClick AddTodo ] [ text "Add Todo" ]
        , div [] [ viewTodos model.todos model.filter ]
        , div [] [ text "Show:", viewFilter "All" model.filter, viewFilter "Active" model.filter, viewFilter "Completed" model.filter ]
        ]
