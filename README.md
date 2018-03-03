# SelectList

A `SelectList` is a nonempty list which always has exactly one element selected.

It is an example of a list [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)).

    myList =
        SelectList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]

    SelectList.before myList
    --> [ 1, 2 ]

    SelectList.after myList
    --> [ 3, 4, 5 ]

    SelectList.selected myList
    --> 3
