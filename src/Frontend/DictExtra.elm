module DictExtra where
import Dict exposing (Dict)
import List

--Helper function for filterMap
maybeInsert : comparable -> Maybe a -> Dict comparable a -> Dict comparable a
maybeInsert k v dict =
  case v of
    Just v' -> Dict.insert k v' dict
    Nothing -> dict

{-| Apply a function that may succeed to all values in the dictionary, but only keep the successes.

-}
filterMap : (comparable -> a -> Maybe b) -> Dict comparable a -> Dict comparable b
filterMap f =
  Dict.foldr maybeInsert Dict.empty << Dict.map f

{-| Keep a key-value pair when its key appears in the second dictionary, then use a function to determine the preference.

-}
intersectWith : (a -> b -> c) -> Dict comparable a -> Dict comparable b -> Dict comparable c
intersectWith f d1 d2 =
  filterMap (\k v -> Maybe.map (f v) <| Dict.get k d2 ) d1

--I need a different type than the core's version
diff' t1 t2 =
  Dict.foldl (\k v t -> Dict.remove k t) t1 t2
