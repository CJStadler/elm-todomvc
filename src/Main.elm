port module Main exposing (Flags, Model, Msg(..), SerializedModel, emptyModel, infoFooter, init, initModel, main, onEnter, serialize, setStorage, update, updateWithStorage, view, viewControls, viewControlsClear, viewControlsCount, viewControlsFilters, viewEntries, viewEntry, viewHeader, viewKeyedEntry, visibilitySwap, visibilityText)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

1.  Model - a full definition of the application's state
2.  Update - a way to step the application state forward
3.  View - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://gnextIde.elm-lang.org/architecture/index.html>

-}

import Browser
import Browser.Dom as Dom
import Date exposing (Date)
import Entry exposing (Entry)
import EntryList exposing (EntryList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Json.Decode as Json
import Maybe
import Task


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • TodoMVC", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : SerializedModel -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage (serialize newModel), cmds ]
    )


type alias SerializedModel =
    { entries : List Entry.Serialized
    , field : String
    , nextId : Int
    , visibility : String
    }


serialize : Model -> SerializedModel
serialize model =
    { entries = List.map Entry.serialize model.entries
    , field = model.field
    , nextId = model.nextId
    , visibility = visibilityText model.visibility
    }


initModel : Flags -> Model
initModel flags =
    let
        serialized =
            Maybe.withDefault emptyModel flags.model

        viz =
            if visibilityText Active == serialized.visibility then
                Active

            else if visibilityText Completed == serialized.visibility then
                Completed

            else
                All
    in
    { entries = List.map Entry.deserialize serialized.entries
    , field = serialized.field
    , nextId = serialized.nextId
    , visibility = viz
    , editingId = Maybe.Nothing
    , currentDate = Date.fromRataDie 0 -- TODO: Handle this better.
    }



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { entries : List Entry
    , field : String
    , nextId : Entry.Id
    , listState : EntryList
    , visibility : Visibility
    , editingId : Maybe Entry.Id
    , currentDate : Date
    }


emptyModel : SerializedModel
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , nextId = 0
    }


type alias Flags =
    { model : Maybe SerializedModel }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags
    , Task.perform SetDate Date.today
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | SetDate Date
    | UpdateField String
    | Add
    | EntryListMsg EntryList.Msg



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetDate date ->
            ( { model | currentDate = date }
            , Cmd.none
            )

        Add ->
            ( { model
                | nextId = model.nextId + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        model.entries
                            ++ [ Entry.new model.field
                                    model.nextId
                                    model.currentDate
                               ]
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        UpdateEntry id desc ->
            let
                updateById entry =
                    if id == Entry.id entry then
                        Entry.update (Entry.Description desc) entry

                    else
                        entry
            in
            ( { model | entries = List.map updateById model.entries }
            , Cmd.none
            )

        Delete id ->
            ( { model | entries = List.filter (\t -> Entry.id t /= id) model.entries }
            , Cmd.none
            )

        DeleteComplete date ->
            ( { model | entries = List.filter (not << Entry.completed) model.entries }
            , Cmd.none
            )

        Check id isCompleted ->
            let
                updateEntry t =
                    if Entry.id t == id then
                        Entry.update (Entry.Completed isCompleted) t

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        CheckAll date isCompleted ->
            let
                updateEntry t =
                    Entry.update (Entry.Completed isCompleted) t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            [ class "todoapp" ]
            [ lazy2 viewHeader model.currentDate model.field
            , viewEntryList model.currentDate model.visibility model.editingId model.entries
            ]
        , infoFooter
        ]


viewHeader : Date -> String -> Html Msg
viewHeader currentDate todoStr =
    let
        dateString =
            Date.format "MMM ddd, y" currentDate

        previousDay =
            SetDate (Date.add Date.Days -1 currentDate)

        nextDay =
            SetDate (Date.add Date.Days 1 currentDate)
    in
    header
        [ class "header" ]
        [ h1 [] [ text dateString ]
        , div []
            [ button [ onClick previousDay ] [ text "Previous" ]
            , button [ onClick nextDay ] [ text "Next" ]
            ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value todoStr
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
