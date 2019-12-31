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
    }


init : Model
init =
    { todos = []
    , newId = 0
    , newDescription = ""
    }


newTodo : Int -> String -> Todo
newTodo newId newDescription =
    Todo newDescription False newId



-- UPDATE


type Msg
    = AddTodo
    | ToggleTodoComplete Int
    | Change String


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


viewTodos : List Todo -> Html Msg
viewTodos todos =
    ul [] (List.map viewTodo todos)


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Enter description", value model.newDescription, onInput Change ] []
        , button [ onClick AddTodo ] [ text "Add Todo" ]
        , div [] [ viewTodos model.todos ]
        ]
