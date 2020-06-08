module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, fieldset, h2, input, label, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)



-- INIT


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { entries = []
    , tempDescription = ""
    , idCounter = 0
    , visibility = All
    }



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
    = CompleteToggled String
    | TodoCommitted
    | TempUpdated String
    | VisibilitySet Visibility



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        CompleteToggled id ->
            { model | entries = List.map (toggleComplete id) model.entries }

        TodoCommitted ->
            { model
                | entries =
                    model.idCounter
                        |> String.fromInt
                        |> String.append "todo_"
                        |> Entry model.tempDescription False
                        |> List.singleton
                        |> List.append model.entries
            }

        TempUpdated description ->
            { model | tempDescription = description }

        VisibilitySet visibility ->
            { model | visibility = visibility }


toggleComplete : String -> Entry -> Entry
toggleComplete id entry =
    if entry.id == id then
        { entry | completed = not entry.completed }

    else
        entry


view : Model -> Html Msg
view model =
    div []
        [ input [ Attr.style "margin" "30px 10px", Attr.placeholder "Add TODO", Attr.value model.tempDescription, onInput TempUpdated ] []
        , button [ onClick TodoCommitted ] [ text "Submit" ]
        , div []
            [ fieldset []
                [ radio "All" (VisibilitySet All)
                , radio "Active" (VisibilitySet Active)
                , radio "Completed" (VisibilitySet Completed)
                ]
            ]
        , h2 [] [ text "Todos" ]
        , renderList model.entries model.visibility
        ]


radio : String -> msg -> Html msg
radio value msg =
    label
        [ Attr.style "padding" "20px" ]
        [ input [ Attr.type_ "radio", Attr.name "visibility", onClick msg ] []
        , text value
        ]


renderList : List Entry -> Visibility -> Html Msg
renderList lst visibility =
    lst
        |> List.filter
            (\e ->
                case visibility of
                    All ->
                        True

                    Active ->
                        not e.completed

                    Completed ->
                        e.completed
            )
        |> List.map
            (\e ->
                div todoStyle
                    [ input [ Attr.type_ "checkbox", Attr.checked e.completed, onClick (CompleteToggled e.id) ] []
                    , span [] [ text e.description ]
                    ]
            )
        |> div [ Attr.class "list" ]


todoStyle : List (Attribute msg)
todoStyle =
    [ Attr.style "padding" "10px"
    , Attr.style "margin" "2px"
    , Attr.style "border" "1px solid #000"
    ]
