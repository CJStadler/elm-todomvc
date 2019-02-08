-- Component representing a list of entries.


module EntryList exposing (EntryList)

import Date exposing (Date)
import Entry exposing (Entry)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)



-- MODEL


type EntryList
    = EntryList Record


type alias Record =
    { visibility : Visibility
    , editingId : Maybe Entry.Id
    , entries : List Entry
    }


type Visibility
    = All
    | Active
    | Completed



-- UPDATE


type Msg
    = Internal InternalMsg
    | External ExternalMsg


type
    InternalMsg
    -- Changes to the UI state.
    = EditingEntry Entry.Id
    | FinishEdit
    | ChangeVisibility Visibility


type
    ExternalMsg
    -- Changes to the application state.
    = Delete Entry.Id
    | UpdateEntry Int String
    | DeleteComplete Date
    | Check Entry.Id Bool
    | CheckAll Date Bool


update : InternalMsg -> EntryList -> ( EntryList, Msg )
update msg (EntryList r) =
    case msg of
        EditingEntry id ->
            let
                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | editingId = Maybe.Just id }
            , Task.attempt (\_ -> NoOp) focus
            )

        FinishEdit ->
            ( { model | editingId = Maybe.Nothing }
            , Cmd.none
            )



-- VIEW


visibilityText : Visibility -> String
visibilityText visibility =
    case visibility of
        All ->
            "All"

        Active ->
            "Active"

        Completed ->
            "Done"


viewEntryList : Date -> Visibility -> Maybe Entry.Id -> List Entry -> Html Msg
viewEntryList currentDate visibility editingId entries =
    let
        onCurrentDate =
            Entry.onDate currentDate

        currentEntries =
            List.filter onCurrentDate entries
    in
    div []
        [ lazy3 viewEntries visibility editingId currentEntries
        , lazy2 viewControls visibility currentEntries
        ]


viewEntries : Visibility -> Maybe Entry.Id -> List Entry -> Html Msg
viewEntries visibility editingId entries =
    let
        isVisible todo =
            case visibility of
                Completed ->
                    Entry.completed todo

                Active ->
                    not (Entry.completed todo)

                All ->
                    True

        visibleEntries =
            List.filter isVisible entries

        allCompleted =
            List.all Entry.completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"

            else
                "visible"

        editingEntry entry =
            case editingId of
                Just id ->
                    id == Entry.id entry

                Nothing ->
                    False
    in
    section
        [ class "main"
        , style "visibility" cssVisibility
        ]
        [ input
            [ class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            , checked allCompleted
            , onClick (CheckAll currentDate (not allCompleted))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] <|
            List.map (\e -> viewKeyedEntry (editingEntry e) e)
                visibleEntries
        ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Bool -> Entry -> ( String, Html Msg )
viewKeyedEntry editing entry =
    ( String.fromInt (Entry.id entry)
    , lazy2 viewEntry editing entry
    )


viewEntry : Bool -> Entry -> Html Msg
viewEntry editing entry =
    let
        entryId =
            Entry.id entry
    in
    li
        [ classList
            [ ( "completed", Entry.completed entry )
            , ( "editing", editing )
            ]
        ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked (Entry.completed entry)
                , onClick (Check entryId (not (Entry.completed entry)))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry entryId) ]
                [ text (Entry.description entry) ]
            , button
                [ class "destroy"
                , onClick (Delete entryId)
                ]
                []
            ]
        , input
            [ class "edit"
            , value (Entry.description entry)
            , name "title"
            , id ("todo-" ++ String.fromInt entryId)
            , onInput (UpdateEntry entryId)
            , onBlur FinishEdit
            , onEnter FinishEdit
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : Visibility -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter Entry.completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy viewControlsCount entriesLeft
        , lazy viewControlsFilters visibility
        , lazy viewControlsClear entriesCompleted
        ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt entriesLeft) ]
        , text (item_ ++ " left")
        ]


viewControlsFilters : Visibility -> Html Msg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" All visibility
        , text " "
        , visibilitySwap "#/active" Active visibility
        , text " "
        , visibilitySwap "#/completed" Completed visibility
        ]


visibilitySwap : String -> Visibility -> Visibility -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text (visibilityText visibility) ]
        ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]
