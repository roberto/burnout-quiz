module Example exposing (..)

import Array
import Expect
import ListIterator exposing (current, hasNext, hasPrevious, next, previous, setCurrent, toArray)
import Test exposing (..)


oneElement : Test
oneElement =
    let
        initList =
            ListIterator.createListIterator "a" []
    in
    describe "ListIterator with one element"
        [ test "current is the unique element" <|
            \_ ->
                Expect.equal "a" <| current initList
        , test "it does not have a next value" <|
            \_ ->
                Expect.equal False <| hasNext initList
        , test "it does not have a previous value" <|
            \_ ->
                Expect.equal False <| hasPrevious initList
        , test "setting the current will replace the element" <|
            \_ ->
                initList
                    |> setCurrent "z"
                    |> toArray
                    |> Array.toList
                    |> Expect.equal [ "z" ]
        , test "calling `next` does nothing" <|
            \_ ->
                initList
                    |> next
                    |> Expect.equal initList
        , test "calling `previous` does nothing" <|
            \_ ->
                initList
                    |> previous
                    |> Expect.equal initList
        ]


threeElements : Test
threeElements =
    let
        initList =
            ListIterator.createListIterator "a" [ "b", "c" ]
    in
    describe "ListIterator with three elements"
        [ describe "initial state"
            [ test "it returns the first element" <|
                \_ ->
                    Expect.equal "a" <| current initList
            , test "it has a next value" <|
                \_ ->
                    Expect.equal True <| hasNext initList
            , test "it does not have a previous value" <|
                \_ ->
                    Expect.equal False <| hasPrevious initList
            , test "setting the current will replace the second element" <|
                \_ ->
                    initList
                        |> setCurrent "z"
                        |> toArray
                        |> Array.toList
                        |> Expect.equal [ "z", "b", "c" ]
            , test "calling previous does nothing" <|
                \_ ->
                    initList
                        |> previous
                        |> Expect.equal initList
            ]
        , let
            list =
                initList |> next
          in
          describe "calling 'next' once"
            [ test "it 'moves' to the second element" <|
                \_ ->
                    Expect.equal "b" <| current list
            , test "it has a next value" <|
                \_ ->
                    Expect.equal True <| hasNext list
            , test "it has a previous value" <|
                \_ ->
                    Expect.equal True <| hasPrevious list
            , test "setting the current will replace the second element" <|
                \_ ->
                    list
                        |> setCurrent "z"
                        |> toArray
                        |> Array.toList
                        |> Expect.equal [ "a", "z", "c" ]
            , test "calling previous goes back to first element" <|
                \_ ->
                    list
                        |> previous
                        |> current
                        |> Expect.equal "a"
            ]
        , let
            list =
                initList |> next |> next
          in
          describe "calling 'next' twice"
            [ test "it 'moves' to the third element" <|
                \_ ->
                    Expect.equal "c" <| ListIterator.current list
            , test "it does not have a next value" <|
                \_ ->
                    Expect.equal False <| ListIterator.hasNext list
            , test "it has a previous value" <|
                \_ ->
                    Expect.equal True <| ListIterator.hasPrevious list
            , test "setting the current will replace the third element" <|
                \_ ->
                    list
                        |> setCurrent "z"
                        |> toArray
                        |> Array.toList
                        |> Expect.equal [ "a", "b", "z" ]
            , test "calling previous goes back to second element" <|
                \_ ->
                    list
                        |> previous
                        |> current
                        |> Expect.equal "b"
            ]
        , let
            list =
                initList |> next |> next |> next
          in
          describe "calling 'next' three times"
            [ test "it 'stops' in the third element" <|
                \_ ->
                    Expect.equal "c" <| ListIterator.current list
            , test "calling previous goes back to second element" <|
                \_ ->
                    list
                        |> previous
                        |> current
                        |> Expect.equal "b"
            ]
        ]
