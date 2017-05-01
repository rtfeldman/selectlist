module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (list, int, tuple, string)
import ZipList
import List.Extra


access : Test
access =
    describe "before, selected, after"
        [ fuzzSegments "before" <|
            \beforeSel sel afterSel ->
                ZipList.fromLists beforeSel sel afterSel
                    |> ZipList.before
                    |> Expect.equal beforeSel
        , fuzzSegments "selected" <|
            \beforeSel sel afterSel ->
                ZipList.fromLists beforeSel sel afterSel
                    |> ZipList.selected
                    |> Expect.equal sel
        , fuzzSegments "after" <|
            \beforeSel sel afterSel ->
                ZipList.fromLists beforeSel sel afterSel
                    |> ZipList.after
                    |> Expect.equal afterSel
        , fuzzSegments "segments" <|
            \beforeSel sel afterSel ->
                ZipList.fromLists beforeSel sel afterSel
                    |> ZipList.segments
                    |> Expect.equal { before = beforeSel, selected = sel, after = afterSel }
        , fuzzSegments "toList" <|
            \beforeSel sel afterSel ->
                ZipList.fromLists beforeSel sel afterSel
                    |> ZipList.toList
                    |> Expect.equal (beforeSel ++ sel :: afterSel)
        ]


transforming : Test
transforming =
    describe "transforming" <|
        [ fuzzSegments "append" <|
            \beforeSel sel afterSel ->
                ZipList.fromLists beforeSel sel afterSel
                    |> ZipList.append beforeSel
                    |> Expect.equal (ZipList.fromLists beforeSel sel (afterSel ++ beforeSel))
        , fuzzSegments "prepend" <|
            \beforeSel sel afterSel ->
                ZipList.fromLists beforeSel sel afterSel
                    |> ZipList.prepend afterSel
                    |> Expect.equal (ZipList.fromLists (afterSel ++ beforeSel) sel afterSel)
        , describe "mapBy"
            [ fuzzSegments "mapBy transforms every element" <|
                \beforeSel sel afterSel ->
                    ZipList.fromLists beforeSel sel afterSel
                        |> ZipList.mapBy (\_ num -> num * sel)
                        |> Expect.equal
                            (ZipList.fromLists
                                (List.map (\num -> num * sel) beforeSel)
                                (sel * sel)
                                (List.map (\num -> num * sel) afterSel)
                            )
            , fuzzSegments "mapBy passes the selected elem" <|
                \beforeSel sel afterSel ->
                    ZipList.fromLists beforeSel sel afterSel
                        |> ZipList.mapBy (\isSelected _ -> isSelected)
                        |> Expect.equal
                            (ZipList.fromLists
                                (List.map (\_ -> False) beforeSel)
                                True
                                (List.map (\_ -> False) afterSel)
                            )
            ]
        , describe "select"
            [ fuzzSegments "is a no-op when trying to select what's already selected" <|
                \beforeSel sel afterSel ->
                    let
                        original =
                            -- make only the selected one negative
                            ZipList.fromLists beforeSel sel afterSel
                                |> ZipList.mapBy
                                    (\isSelected elem ->
                                        if isSelected then
                                            negate elem
                                        else
                                            elem
                                    )
                    in
                        original
                            |> ZipList.select (\num -> num < 0)
                            |> Expect.equal original
            , fuzzSegments "is a no-op when the predicate fails every time" <|
                \beforeSel sel afterSel ->
                    let
                        original =
                            ZipList.fromLists beforeSel sel afterSel
                    in
                        original
                            |> ZipList.select (\num -> num < 0)
                            |> Expect.equal original
            , fuzzSegments "selects the first one it finds" <|
                \beforeSel sel afterSel ->
                    let
                        predicate num =
                            num > 5

                        firstInList =
                            (beforeSel ++ sel :: afterSel)
                                |> List.Extra.find predicate
                                |> Maybe.withDefault sel
                    in
                        ZipList.fromLists beforeSel sel afterSel
                            |> ZipList.select predicate
                            |> ZipList.selected
                            |> Expect.equal firstInList
            , describe "selects the first one it finds in a hardcoded list"
                [ test "where it's the beginning of the `before` list" <|
                    \() ->
                        ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 2, 1 ]
                            |> ZipList.select (\num -> num > 0)
                            |> Expect.equal (ZipList.fromLists [] 1 [ 2, 3, 4, 5, 2, 1 ])
                , test "where it's the middle of the `before` list" <|
                    \() ->
                        ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 2, 1 ]
                            |> ZipList.select (\num -> num > 1)
                            |> Expect.equal (ZipList.fromLists [ 1 ] 2 [ 3, 4, 5, 2, 1 ])
                , test "where it's the end of the `before` list" <|
                    \() ->
                        ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 2, 1 ]
                            |> ZipList.select (\num -> num > 2)
                            |> Expect.equal (ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 2, 1 ])
                , test "where it's the selected element in the list" <|
                    \() ->
                        ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 2, 1 ]
                            |> ZipList.select (\num -> num > 3)
                            |> Expect.equal (ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 2, 1 ])
                , test "where it's the beginning of the `after` list" <|
                    \() ->
                        ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 2, 5, 1 ]
                            |> ZipList.select (\num -> num > 4)
                            |> Expect.equal (ZipList.fromLists [ 1, 2, 3, 4 ] 5 [ 2, 5, 1 ])
                , test "where it's the middle of the `after` list" <|
                    \() ->
                        ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 2, 5, 6, 1, 6, 7 ]
                            |> ZipList.select (\num -> num > 5)
                            |> Expect.equal (ZipList.fromLists [ 1, 2, 3, 4, 5, 2, 5 ] 6 [ 1, 6, 7 ])
                , test "where it's the end of the `after` list" <|
                    \() ->
                        ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 2, 5, 6, 1, 6, 7 ]
                            |> ZipList.select (\num -> num > 6)
                            |> Expect.equal (ZipList.fromLists [ 1, 2, 3, 4, 5, 2, 5, 6, 1, 6 ] 7 [])
                ]
            ]
        ]


{-| Choose positive ints so that we can throw a negative one in there and
detect it later.
-}
fuzzSegments : String -> (List Int -> Int -> List Int -> Expectation) -> Test
fuzzSegments =
    fuzz3 (list positiveInt) positiveInt (list positiveInt)


positiveInt : Fuzz.Fuzzer Int
positiveInt =
    Fuzz.map abs int
