module ListIterator exposing (ListIterator, createListIterator, current, hasNext, hasPrevious, next, previous, setCurrent, toArray, toList)

import Array exposing (Array)


type ListIterator a
    = ListIterator (Array a) Int a


createListIterator : a -> List a -> ListIterator a
createListIterator head tail =
    ListIterator (Array.fromList (head :: tail)) 0 head


current : ListIterator a -> a
current (ListIterator list index fallback) =
    Maybe.withDefault fallback <| Array.get index list


next : ListIterator a -> ListIterator a
next ((ListIterator list index fallback) as listIterator) =
    if hasNext listIterator then
        ListIterator list (index + 1) fallback

    else
        listIterator


hasNext : ListIterator a -> Bool
hasNext (ListIterator list index _) =
    index /= (Array.length list - 1)


previous : ListIterator a -> ListIterator a
previous ((ListIterator list index fallback) as listIterator) =
    if hasPrevious listIterator then
        ListIterator list (index - 1) fallback

    else
        listIterator


hasPrevious : ListIterator a -> Bool
hasPrevious (ListIterator _ index _) =
    index /= 0


setCurrent : a -> ListIterator a -> ListIterator a
setCurrent element (ListIterator list index fallback) =
    let
        newArray =
            Array.set index element list
    in
    if index == 0 then
        ListIterator newArray index element

    else
        ListIterator newArray index fallback


toArray : ListIterator a -> Array a
toArray (ListIterator list _ _) =
    list


toList : ListIterator a -> List a
toList (ListIterator list _ _) =
    Array.toList list
