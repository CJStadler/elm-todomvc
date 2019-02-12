module Helpers exposing (importEntriesSince, onEnter)

import Date exposing (Date)
import Entry exposing (Entry)
import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


importEntriesSince : Date -> Date -> List Entry -> List Entry
importEntriesSince from to entries =
    -- Move incomplete entries on or after `from` to `to`.
    let
        updateIfInRange e =
            if
                not (Entry.completed e)
                    && Date.isBetween from to (Entry.date e)
            then
                Entry.new (Entry.description e) (Entry.id e) to

            else
                e
    in
    List.map updateIfInRange entries
