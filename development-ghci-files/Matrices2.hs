-- This is a module for building Matrices, with Maps!
-- (Should be less awkward than with Lists)
module Matrices2 where

    -- We import a module 
    -- Use ":browse <module>"
    -- In GHCI to see all the functions defined in <module>
    import Data.Map
    
    -- We create an explicit type for the position
    type Position = (Int, Int)

    -- This is the type for matrices, 
    -- We use the type Map from Data.Map
    -- In GHCI use ":info <type>"
    -- To see all info about the type <type>
    -- For example: :info Map
    type Matrix a = Map Position a 

    emptyMatrix :: Matrix a 
    emptyMatrix = empty -- 'empty' is defined in Data.Map (empty map)

    -- get returns a 'Maybe a', which can be either
    -- Nothing -- if the position was not set to anything
    -- Just <a value>  -- if it was
    get :: Matrix a -> Position -> Maybe a
    get m (x, y) = m !? (x,y) -- We use (!?) which fetches the value for the key (x,y) (and returns Nothing if it's not in the map)

    -- Setting is just inserting a key-value pair in the Map
    set :: Matrix a -> Position -> a -> Matrix a
    set m (x, y) value = insert (x, y) value m

    -- >>> get emptyMatrix (0, 0)
    -- Nothing
    -- >>> let m = set emptyMatrix (1, 1) "X"
    -- >>> get m (1, 1)
    -- Just "X"
    example :: Maybe String
    example = 
        let matrixWithX = set emptyMatrix (1, 1) "X"
        in get matrixWithX (0, 0) -- Should be "Nothing"
