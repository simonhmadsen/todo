module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, button, div, text, input, h2, span, label, fieldset)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


-- INIT

main = Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

init : Model
init =
  Model [] "" 0 All

-- MODEL

type alias Model =
    { entries : List Entry 
    , tempDescription : String
    , idCounter : Int
    , visibility : Visibility
    }

type alias Entry =
    { description : String
    , completed : Bool
    , id : String
    }

type Visibility 
    = All
    | Active
    | Completed

-- MESSAGE

type Msg 
    = ToggleComplete String
    | Commit
    | UpdateTemp String
    | SetVisibility String

-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleComplete id ->
            Model 
                (List.map (\e -> toggleComplete e id) model.entries)
                model.tempDescription
                model.idCounter
                model.visibility

        Commit ->
            Model (List.append model.entries
                <| List.singleton
                <| Entry model.tempDescription False
                <| String.append "todo_"
                <| String.fromInt
                <| model.idCounter)
                 ""
                 (model.idCounter + 1)
                 model.visibility

        UpdateTemp description ->
            { model | tempDescription = description }

        SetVisibility visibility ->
            case visibility of
                "All" -> { model | visibility = All }
                "Active" -> { model | visibility = Active }
                "Completed" -> { model | visibility = Completed }
                _ -> model

toggleComplete : Entry -> String -> Entry
toggleComplete entry id =
    if entry.id == id then
        { entry | completed = not entry.completed }
    else 
        entry


view : Model -> Html Msg
view model =
    div []
        [ input [ style "margin" "30px 10px", placeholder "Add TODO", value model.tempDescription, onInput UpdateTemp ] []
        , button [ onClick Commit ] [ text "Submit" ]
        , div [] 
            [ fieldset []
                [ radio "All" (SetVisibility "All")
                , radio "Active" (SetVisibility "Active")
                , radio "Completed" (SetVisibility "Completed")
                ]
            ]
        , h2 [] [ text "Todos"]
        , renderList model.entries model.visibility
        ]

radio : String -> msg -> Html msg
radio value msg =
  label
    [ style "padding" "20px" ]
    [ input [ type_ "radio", name "visibility", onClick msg ] []
    , text value
    ]


renderList : List Entry -> Visibility -> Html Msg
renderList lst visibility =
    lst
        |> List.filter (\e ->
            case visibility of
                All -> True
                Active -> e.completed /= True
                Completed -> e.completed == True               
        )
        |> List.map (\e -> 
            div todoStyle
                [ input [ type_ "checkbox", checked e.completed, onClick (ToggleComplete e.id)] []
                , span [] [ text e.description ]
                ]
        )
       |> div [ class "list" ]

todoStyle : List (Attribute msg)
todoStyle =
    [ style "padding" "10px"
    , style "margin" "2px"
    , style "border" "1px solid #000" 
    ]