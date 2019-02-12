module Tests exposing (suite)

import Date exposing (Date)
import Entry exposing (new)
import Expect exposing (Expectation)
import Helpers exposing (importEntriesSince)
import Test exposing (..)


suite : Test
suite =
    describe "importEntriesSince"
        [ test "Doesn't update any when last = today" <|
            \_ ->
                let
                    date =
                        Date.fromRataDie 123

                    entries =
                        [ Entry.new "Foo" 1 (Date.fromRataDie 122)
                        , Entry.new "Foo" 2 (Date.fromRataDie 122)
                        , Entry.new "Foo" 3 (Date.fromRataDie 123)
                        ]
                in
                importEntriesSince date date entries
                    |> Expect.equal entries
        , test "Updates incomplete entries from `from` date" <|
            \_ ->
                let
                    from =
                        Date.fromRataDie 123

                    to =
                        Date.fromRataDie 124

                    entries =
                        [ Entry.new "Foo" 1 (Date.fromRataDie 122)
                        , Entry.new "Foo" 2 (Date.fromRataDie 123)
                        , Entry.new "Foo" 3 (Date.fromRataDie 124)
                        , Entry.new "Foo" 4 (Date.fromRataDie 123)
                            |> Entry.update (Entry.Completed True)
                        ]

                    expected =
                        [ Entry.new "Foo" 1 (Date.fromRataDie 122)
                        , Entry.new "Foo" 2 (Date.fromRataDie 124)
                        , Entry.new "Foo" 3 (Date.fromRataDie 124)
                        , Entry.new "Foo" 4 (Date.fromRataDie 123)
                            |> Entry.update (Entry.Completed True)
                        ]
                in
                importEntriesSince from to entries
                    |> Expect.equal expected
        , test "Updates incomplete entries between `from` and `to`" <|
            \_ ->
                let
                    from =
                        Date.fromRataDie 122

                    to =
                        Date.fromRataDie 124

                    entries =
                        [ Entry.new "Foo" 1 (Date.fromRataDie 122)
                        , Entry.new "Foo" 2 (Date.fromRataDie 123)
                        , Entry.new "Foo" 3 (Date.fromRataDie 124)
                        , Entry.new "Foo" 4 (Date.fromRataDie 123)
                            |> Entry.update (Entry.Completed True)
                        ]

                    expected =
                        [ Entry.new "Foo" 1 (Date.fromRataDie 124)
                        , Entry.new "Foo" 2 (Date.fromRataDie 124)
                        , Entry.new "Foo" 3 (Date.fromRataDie 124)
                        , Entry.new "Foo" 4 (Date.fromRataDie 123)
                            |> Entry.update (Entry.Completed True)
                        ]
                in
                importEntriesSince from to entries
                    |> Expect.equal expected
        ]
