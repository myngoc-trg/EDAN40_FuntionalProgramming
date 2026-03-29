-- Now, A Matrix as a persistent data structure (we keep the history of the matrix)
-- (When we do it this way, most functions are easy to write!)
module Matrices3 where

    import Data.List

    -- Type for positions is the same
    type Position = (Int, Int)

    -- Type for Changes
    -- ((0,0), "X") means we changed the cell (0,0) to X
    -- ((1,1)), "O") means we changed the cell (1, 1) to O 
    -- A change is a tuple
    type Change a = (Position, a)
    
    -- A matrix is a list of all the changes
    type Matrix a = [Change a]

    -- An empty matrix is just an empty history
    emptyMatrix :: Matrix a 
    emptyMatrix = []

    -- Changing a cell is just adding a change 
    -- To the history
    -- Examples:
    -- >>> set emptyMatrix (0, 0) "X"
    -- [((0,0),"X")]
    -- >>> let m = set emptyMatrix (0, 0) "X"
    -- >>> set m (1, 1) "O"
    -- [((1,1),"O"),((0,0),"X")]
    set :: Matrix a -> Position -> a -> Matrix a
    set m p v = 
        let newChange = (p, v)
        in newChange : m

    -- This is useful for getting
    -- Returns true of the change is at the right position
    -- >>> hasPosition (0,0) ((0,0), "X")
    -- True
    -- >>> hasPosition (1,1) ((0,0), "X")
    -- False
    hasPosition :: Position -> Change a -> Bool
    hasPosition pos (pos1,v) = pos == pos1

    -- Getting is finding the latest change
    -- That happened at position (x, y)
    -- >>> let m = set emptyMatrix (1, 1) "X"
    -- >>> get m (1, 1)
    -- Just "X"
    get :: Matrix a -> Position -> Maybe a 
    get [] _ = Nothing
    get history position = 
        -- We use a case ... of syntax, because find returns a maybe
        case find (hasPosition position) history of 
            Nothing -> Nothing
            -- We use pattern matching to extract the 
            Just (p, v) -> Just v

    -- Then, canceling an action is very easy.
    -- We just drop the latest change to the history
    cancel :: Matrix a -> Matrix a 
    cancel = drop 1 -- equivalent to "cancel m = drop 1 m" (point-free style)
