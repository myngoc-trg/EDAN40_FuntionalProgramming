-- A module that implements a Matrix with nested lists
module Matrices1 where

    -- We create a type synonym, a matrix is a list of lists
    -- of as
    -- (a is a type variable, coult be any type)
    -- Matrix Int <- A Matrix of ints
    -- Matrix String <- A matrix of strings
    type Matrix a = [[a]]

    -- An empty matrix
    -- Example ('>>>' means 'in ghci')
    -- >>> emptyMatrix 3 3 "-"
    -- [["-","-","-"],["-","-","-"],["-","-","-"]]
    -- >>> emptyMatrix 3 3 0 
    -- [[0,0,0],[0,0,0],[0,0,0]]
    emptyMatrix :: Int -> Int -> a -> Matrix a
    emptyMatrix w h emptyCell = [emptyRow | i <- [0..(h-1)]]
        where emptyRow = [emptyCell | j <- [0..(w-1)]]

    -- We can use GHCI to get the type of any Haskell expression
    -- >>> :type emptyMatrix
    -- emptyMatrix :: Int -> Int -> a -> Matrix a

    -- When we set the size of matrix, we only need to 
    -- provide the value for the empty cell
    -- >>> :type emptyMatrix 3 3 
    -- emptyMatrix 3 3 :: a -> Matrix a

    -- Get the value of a cell, given the matrix
    -- And a position with two integers.
    -- We use list indexing: (!!)
    -- >>> [1, 2, 3, 4] !! 2
    -- 3
    get :: Matrix a -> Int -> Int -> a
    get m i j = (m !! i) !! j

    -- This code is supposed to be ugly
    -- To change a value in the matrix, we need to build a brand new 
    -- matrix, with one row changed
    -- To change a value in the row, we need to make a new row with one value changed
    set :: Matrix a -> Int -> Int -> a -> Matrix a 
    set matrix i j cell = 
        let selectedRow = matrix !! i -- We get the row at pos i
            -- we make a function newRow which updates a provided row called 'row'
            newRow row = take j row ++ [cell] ++ drop (j+1) row
            -- we make a function newMat which updates a matrix called 'mat'
            -- Uses 'newRow' defined above
            newMat mat = take i mat ++ [newRow selectedRow] ++ drop (i+1) mat
        in newMat matrix

    -- Example for using the get and set operations
    example :: Bool
    example = 
        let start = emptyMatrix 3 3 "-"
            step1M = set start 0 0 "X" 
            step2M = set step1M 1 1 "X"
        in (get step2M 0 0) == "X"
