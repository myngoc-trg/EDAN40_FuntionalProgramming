import Data.List
import Data.Maybe
import Data.Foldable (Foldable(toList))

-- 2.1 Propositional Logic
-- 1. Design a data type Proposition to represent propositions
data Proposition = Var String 
                | Proposition :&: Proposition
                | Proposition :|: Proposition
                | Not Proposition
                deriving (Eq, Show)

-- 2. Define a function
-- vars :: Proposition-> [String]
-- which returns a list of the variables in a proposition. Make sure each variable appears only once in the list you return.
vars :: Proposition -> [String]
vars (Var x) = [x]
vars (p :&: q) = vars p `union` vars q
vars (p :|: q) = vars p `union` vars q
vars (Not p) = vars p

-- Suppose you are given a list of variable names and their values, of type Bool, for example, [("p",True),("q",False)]. 
-- Define a function
-- truthValue :: Proposition-> [(String,Bool)]-> Bool
-- which determines whether the proposition is true when the variables have the values given
truthValue :: Proposition -> [(String, Bool)] -> Bool
truthValue (Var x) val = fromJust (lookup x val)
truthValue (p :&: q) val = truthValue p val && truthValue q val
truthValue (p :|: q) val = truthValue p val || truthValue q val
truthValue (Not p) val = not (truthValue p val)

truthValue2 :: Proposition -> [(String, Bool)] -> Bool
truthValue2 (Var p) val =
    case lookup p val of
        Just value -> value
        Nothing -> error ("Variable not found: " ++ p)
truthValue2 (p :&: q) val = truthValue2 p val && truthValue2 q val
truthValue2 (p :|: q) val = truthValue2 p val || truthValue2 q val
truthValue2 (Not p) val = not (truthValue2 p val)

--3. Define a function
-- tautology :: Proposition-> Bool
-- which returns true if the proposition holds for all values of the variables appearing in it.
allvars :: [String] -> [[(String, Bool)]]
allvars [] = [[]]
allvars (x:xs) = [(x, b):val | val <- allvars xs, b <- [True, False]]

taupology :: Proposition -> Bool
taupology a =
    and [ truthValue a val | val <- allvars (vars a) ]



-- 2.2 File Systems (TDA555)
-- A file either contains data or is a directory. A directory contains other files (which may themselves be directories) along with a name for each one.
-- 1. Design a data type to represent the contents of a directory. Ignore the contents of files: you are just trying to represent file names and the way they are organised into directories here
data File = File String | Dir String [File]
            deriving (Eq, Show)

-- 2. Define a function to search for a given file name in a directory. You should return a path leading to a file with the given name. 
-- Thus if your directory contains a, b, and c, and b is a directory containing x and y, then searching for x should produce b/x.
type FileSystem = [File]
search :: FileSystem -> String -> [String]
search files name = 
    [name | File name' <- files, name == name']
    ++
    [dir_name ++ "/" ++ path | Dir dir_name files' <- files, path <- search files' name]

searchMaybe :: FileSystem -> String -> Maybe String
searchMaybe files name =
    listToMaybe (
        [name | File name' <- files, name == name']
        ++
        [dir_name ++ "/" ++ path | Dir dir_name files' <- files, Just path <- [searchMaybe files' name]]
    )

searchMaybe2 :: FileSystem -> String -> Maybe String
searchMaybe2 files name = listToMaybe (search files name)



-- 2.3 Sets
-- 1. Design a datastructure for sets . I.e. there should be a type Set a, and a number of functions for creating, combining, and investigating sets. 
--There should at least be a function to create an empty set, add an element to a set, take the union of two sets, remove an element from the set, and check if an element is in the set.
data Set a = Set [a]
    deriving (Show)

emptySet :: Set a
emptySet = Set []

addToSet :: Eq a => a -> Set a -> Set a
addToSet x (Set xs) 
    | x `elem` xs = Set xs
    | otherwise = Set (x:xs)

removeFromSet :: Eq a => a -> Set a -> Set a 
removeFromSet x (Set xs) = Set (xs \\ [x]) 

unionSet :: Eq a => Set a -> Set a -> Set a
unionSet (Set xs) (Set ys) = Set (xs `union` ys)

checkMemberInSet :: Eq a => a -> Set a -> Bool
checkMemberInSet x (Set xs) = elem x xs

isEmptySet :: Set a -> Bool
isEmptySet (Set xs) = null xs



-- 2.4 Ordering (Thompson)
-- Complete the following instance declarations:
-- instance (Ord a, Ord b) => Ord (a,b) where ...
-- instance Ord b => Ord [b] where ...
-- where pairs and lists should be ordered lexicographically, like the words in dictionary

-- instance (Ord a, Ord b) => Ord (a, b) where
--      (x,y) < (z,w) = x<z || x==Z && y<w
--      (x,y) <= (z,w) = x<z || x==z && y<=w
--      (x,y) > (z,w) = x>z || x==z && y>w    
--      (x,y) >= (z,w) = x>z || x==z && y>=w  
--      max (x,y) (z,w)
--          | (x,y) >= (z,w) = (x,y)
--          | otherwise = (z,w)
--      min (x,y) (z,w)
--          | (x,y) <= (z,w) = (x,y)
--          | otherwise = (z,w)
--      compare (x,y) (z,w)
--          | (x,y) == (z,w) = EQ
--          | (x,y) < (z,w) = LT
--          | (x,y) > (z,w) = GT

-- instance Ord b => Ord [b] where
--  [] < _ = True
--  _ < [] = False
-- (x:xs) < (y:ys) = x < y || x=y && xs < ys
-- x <= y = x<y || x==y
-- x>y = y<x
-- x >= y = y <= x
-- max x y 
--  | x >= y = x
--  | otherwise = y
-- min x y 
--  | x >= y = y
--  | otherwise = x
-- compare x y 
--  | x == y = EQ
--  | x > y = GT
--  | x < y = LT




-- 2.5 ListNatural (lecture)
newtype ListNatural = LN [()]
    deriving (Eq, Show)

zeroLN = LN []
checkZeroLN (LN x) = (x == [])

plusLN (LN x) (LN y) = LN (x++y)

incLN (LN x) = LN (():x)

decLN (LN x) 
    | checkZeroLN (LN x) = error "no negative naturals exist"
    | otherwise = LN (tail x)

minusLN (LN x) (LN y) 
    | length x < length y = error "no negative naturals exist"
    | checkZeroLN (LN y) = LN x
    | otherwise = minusLN (decLN (LN x)) (decLN (LN y))

timesLN (LN x) (LN y) = LN (concatMap (const x) y)
absLN (LN x) = LN x

signumLN (LN x)
    | checkZeroLN (LN x) = zeroLN
    | otherwise = incLN zeroLN

toInterger (LN x) = length x

fromInteger x
    | x < 0 = error "no negative naturals exist"
    | x == 0 = zeroLN
    | otherwise = LN (toList x) where
        toList 0 = []
        toList n = toList (n-1)

instance Num ListNatural where
    (+) = plusLN
    (-) = minusLN
    (*) = timesLN
    abs = absLN
    signum = signumLN



