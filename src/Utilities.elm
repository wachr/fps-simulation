module Utilities exposing (..)

filterBy : (a -> Bool) -> a -> Maybe a
filterBy predicate operand =
    if predicate operand then
        Just operand

    else
        Nothing
