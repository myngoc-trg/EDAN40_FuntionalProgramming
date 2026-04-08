-- class1.hs
-- some solutions to class 1 exercises, EDAN40

-- maxi x y
maxi :: Ord a => a -> a -> a
maxi x y 
  | x <= y    = y
  | otherwise = x

-- sumsq
-- sumsq :: Num a => a -> a
-- variant1 (recursive, cases)
sumsq1 0 = 0
sumsq1 n = n^2 + sumsq1 (n - 1)
-- problem with negative stuff, non-integer stuff, ...
-- if we say Int -> Int we still have negative stuff, so cases are necessary

sumsq2 :: Int -> Int
sumsq2 n
  | n <= 0     = 0
  | otherwise  = n^2 + sumsq1 (n - 1)

-- now mapping and folding
sumsq3 :: Int -> Int
sumsq3 n
  | n <= 0     = 0
  | otherwise  = foldl (+) 0 (map (^2) [1..n])

sumsq4 :: Int -> Int
sumsq4 n
  | n <= 0     = 0
  | otherwise  = foldl1 (+) (map (^2) [1..n])

sumsq5 :: Int -> Int
sumsq5 n
  | n <= 0     = 0
  | otherwise  = foldr1 (+) (map (^2) [1..n])


-- hanoi
hanoi :: Integer -> Integer
hanoi 0 = 0
hanoi n = 1 + 2 * hanoi (n-1)

-- for correctness: test if non-negative
hanoi1 :: Integer -> Integer
hanoi1 n
  | n <= 0 = 0
  | otherwise = 1 + 2 * hanoi (n-1)

-- factors
nextFactor :: Int -> Int -> Int
smallestFactor :: Int -> Int

-- smallestFactor n = nextFactor 1 n
smallestFactor = nextFactor 1

nextFactor k n 
  | k >= n            = n
  | mod n (k+1) == 0  = k+1
  | otherwise         = nextFactor (k+1) n

numFactors :: Int -> Int
-- first, find factors, then count them
numFactors n = 
 length $ removeDuplicates $ map (flip nextFactor n) [1..n]

-- avoiding duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs)
  | elem x xs  = removeDuplicates xs
  | otherwise  = x : removeDuplicates xs

-- types
type Month = Integer
daysInMonth :: Month -> Integer -> Integer
daysInMonth m y
  | m <= 0 || m > 12   = 0
  | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12  = 31
  | m == 4 || m == 6 || m == 9 || m == 11   = 30
  | mod y 4 == 0  = 29
  | otherwise     = 28

data Date = Date Integer Month Integer

validDate :: Date -> Bool
validDate (Date d m y)
  | (d > 0) && (d <= daysInMonth m y) = True
  | otherwise                         = False 

-- multiplying

multiply :: Num a => [a] -> a
multiply = foldr1 (*) 

-- substitution
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y (z:ys) 
 | y == z    = x: (substitute x y ys)
 | otherwise = z: (substitute x y ys)

-- duplicates
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates [x] = False
duplicates (x:xs) 
 | elem x xs = True
 | otherwise = duplicates xs

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))
-- obiously wrong! RHS always true if removeDuplicates returns a list without duplicates, e.g. []

-- comprehensions
-- cartesian product

pyth n = [(a,b,c)|a<-[1..n],b<-[a..n],c<-[b..n],a^2 + b^2 == c^2]

-- permutations
remove :: Eq a => a -> [a] -> [a]
remove e [] = []
remove e (x:xs)
 | e == x    = remove e xs
 | otherwise = x : (remove e xs)

remove1 :: Eq a => a -> [a] -> [a]
remove1 e [] = []
remove1 e (x:xs)
 | e == x    =  xs
 | otherwise = x : (remove1 e xs)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _  = False
isPermutation _  [] = False
isPermutation (x:xs) ys
 | elem x ys = isPermutation xs (remove1 x ys) 
 | otherwise = False

-- shortest and longest
shortestAndLongest [] = ([],[])
shortestAndLongest xs = (theShortest xs, theLongest xs)

theShortest (x:xs) 
 | null xs || shorter x xs = x
 | otherwise    = theShortest xs

theLongest (x:xs) 
 | null xs || longer x xs = x
 | otherwise    = theLongest xs

shorter x xs = (length x) <= (minimum $ map length xs)
longer y ys = (length y) >= (maximum $ map length ys)

-- mystery
mystery xs = foldr (++) [] (map (\y -> [y]) xs)
-- is an identity on lists