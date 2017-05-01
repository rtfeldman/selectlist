module ZipList
    exposing
        ( ZipList
        , mapBy
        , segments
        , fromLists
        , select
        , prepend
        , append
        , before
        , after
        , selected
        , toList
        , singleton
        )

{-| A ZipList is a nonempty list which always has exactly one element selected.

@docs ZipList, fromLists, singleton


## Reading

@docs segments, toList, before, selected, after


## Transforming

@docs mapBy, select, append, prepend

-}


{-| A nonempty list which always has exactly one element selected.

Create one using [`fromLists`](#fromLists).

-}
type ZipList a
    = ZipList (List a) a (List a)


{-| Return the elements before the selected element.

    import ZipList

    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> ZipList.before

    == [ 1, 2 ]

-}
before : ZipList a -> List a
before (ZipList beforeSel _ _) =
    beforeSel


{-| Return the elements after the selected element.

    import ZipList

    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> ZipList.after

    == [ 3, 4, 5 ]

-}
after : ZipList a -> List a
after (ZipList _ _ afterSel) =
    afterSel


{-| Return the selected element.

    import ZipList

    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> ZipList.selected

    == 3

-}
selected : ZipList a -> a
selected (ZipList _ sel _) =
    sel


{-| A ZipList containing exactly one element.

    import ZipList

    ZipList.singleton "foo"

    == ZipList.fromLists [] "foo" []

-}
singleton : a -> ZipList a
singleton sel =
    ZipList [] sel []


{-| Transform each element of the ZipList. The transform
function receives a `Bool` which is `True` if it was passed
the ZipList's selected element.

    import ZipList

    doubleOrNegate isSelected num =
        if isSelected then
            num * -1
        else
            num * 2


    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> ZipList.mapBy doubleOrNegate

    == ZipList.fromLists [ 2, 4 ] -3 [ 8, 10, 12 ]

-}
mapBy : (Bool -> a -> b) -> ZipList a -> ZipList b
mapBy transform (ZipList beforeSel sel afterSel) =
    ZipList
        (List.map (transform False) beforeSel)
        (transform True sel)
        (List.map (transform False) afterSel)


{-| Returns the ZipList's selected element, along with lists of
the elements before and after it.

    import ZipList

    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> ZipList.segments

    == { before = [ 1, 2 ], selected = 3, after = [ 4, 5, 6 ] }

-}
segments : ZipList elem -> { before : List elem, selected : elem, after : List elem }
segments (ZipList beforeSel sel afterSel) =
    { before = beforeSel, selected = sel, after = afterSel }


{-| Returns a ZipList.

Use [`segments`](#segments) to reverse this operation.

    import ZipList

    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> ZipList.segments

    == { before = [ 1, 2 ], selected = 3, after = [ 4, 5, 6 ] }

-}
fromLists : List a -> a -> List a -> ZipList a
fromLists =
    ZipList


{-| Change the selected element to the first one which passes a
predicate function. If no elements pass, the ZipList is unchanged.

    import ZipList

    isEven num =
        num % 2 == 0


    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> ZipList.select isEven

    == ZipList.fromLists [ 1 ] 2 [ 3, 4, 5, 6 ]

-}
select : (a -> Bool) -> ZipList a -> ZipList a
select isSelectable ((ZipList beforeSel sel afterSel) as original) =
    case selectHelp isSelectable beforeSel sel afterSel of
        Nothing ->
            original

        Just ( newBefore, newSel, newAfter ) ->
            ZipList newBefore newSel newAfter


selectHelp : (a -> Bool) -> List a -> a -> List a -> Maybe ( List a, a, List a )
selectHelp isSelectable beforeList selectedElem afterList =
    case ( beforeList, afterList ) of
        ( [], [] ) ->
            Nothing

        ( [], first :: rest ) ->
            if isSelectable selectedElem then
                Just ( beforeList, selectedElem, afterList )
            else if isSelectable first then
                Just ( beforeList ++ [ selectedElem ], first, rest )
            else
                case selectHelp isSelectable [] first rest of
                    Nothing ->
                        Nothing

                    Just ( newBefore, newSelected, newAfter ) ->
                        Just ( selectedElem :: newBefore, newSelected, newAfter )

        ( first :: rest, _ ) ->
            if isSelectable first then
                Just ( [], first, (rest ++ selectedElem :: afterList) )
            else
                case selectHelp isSelectable rest selectedElem afterList of
                    Nothing ->
                        Nothing

                    Just ( newBefore, newSelected, newAfter ) ->
                        Just ( first :: newBefore, newSelected, newAfter )


{-| Add elements to the end of a ZipList.

    import ZipList

    ZipList.fromLists [ 1, 2 ] 3 [ 4 ]
        |> ZipList.append [ 5, 6 ]

    == ZipList.fromLists [ 1 ] 2 [ 3, 4, 5, 6 ]

-}
append : List a -> ZipList a -> ZipList a
append list (ZipList beforeSel sel afterSel) =
    ZipList beforeSel sel (afterSel ++ list)


{-| Add elements to the beginning of a ZipList.

    import ZipList

    ZipList.fromLists [ 3 ] 4 [ 5, 6 ]
        |> ZipList.prepend [ 1, 2 ]

    == ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 6 ]

-}
prepend : List a -> ZipList a -> ZipList a
prepend list (ZipList beforeSel sel afterSel) =
    ZipList (list ++ beforeSel) sel afterSel


{-| Return a `List` containing the elements in a ZipList.

    import ZipList

    ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 6 ]
        |> ZipList.toList

    == [ 1, 2, 3, 4, 5, 6 ]

-}
toList : ZipList a -> List a
toList (ZipList beforeSel sel afterSel) =
    beforeSel ++ sel :: afterSel
