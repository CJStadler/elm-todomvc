module Entry exposing
    ( Entry
    , Msg(..)
    , Serialized
    , completed
    , date
    , description
    , deserialize
    , editing
    , id
    , new
    , serialize
    , update
    )

import Date exposing (Date)



-- MODEL


type Entry
    = Entry Record


type alias Record =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    , date : Date
    }


type alias Serialized =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    , date : Int
    }


new : String -> Int -> Date -> Entry
new desc uid d =
    Entry
        { description = desc
        , completed = False
        , editing = False
        , id = uid
        , date = d
        }


description : Entry -> String
description (Entry r) =
    r.description


completed : Entry -> Bool
completed (Entry r) =
    r.completed


editing : Entry -> Bool
editing (Entry r) =
    r.editing


id : Entry -> Int
id (Entry r) =
    r.id


date : Entry -> Date
date (Entry r) =
    r.date


serialize : Entry -> Serialized
serialize (Entry r) =
    { description = r.description
    , completed = r.completed
    , editing = r.editing
    , id = r.id
    , date = Date.toRataDie r.date
    }


deserialize : Serialized -> Entry
deserialize e =
    Entry
        { description = e.description
        , completed = e.completed
        , editing = e.editing
        , id = e.id
        , date = Date.fromRataDie e.date
        }



-- UPDATE


type Msg
    = Description String
    | Completed Bool
    | Editing Bool


update : Msg -> Entry -> ( Entry, Cmd Msg )
update change (Entry r) =
    case change of
        Description d ->
            ( Entry { r | description = d }
            , Cmd.none
            )

        Completed c ->
            ( Entry { r | completed = c }
            , Cmd.none
            )

        Editing e ->
            --            let
            --               focus =
            --                  Dom.focus ("todo-" ++ String.fromInt id)
            --         in
            ( Entry { r | editing = e }
            , Cmd.none
            )
