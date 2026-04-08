
--2.1 Ex

--2.1.1: The maximum function
maxi :: Ord a => a -> a -> a
maxi x y
    | x >= y = x
    | otherwise = y



--2.1.2: Sum of squares
--recursion
sumsq1 0 = 0
sumsq1 n = n*n + sumsq1 (n-1)

sumsq2 :: Int -> Int
sumsq2 n
    | n <= 0 = 0
    | otherwise = n*n + sumsq1 (n-1)
-- mapping and folding
sumsq3 :: Int -> Int
sumsq3 n
    | n <= 0 = 0
    | otherwise = foldl (+) 0 (map (^2) [1..n])
-- 0 is the starting value
-- left
sumsq4 :: Int -> Int
sumsq4 n
    | n <= 0 = 0
    | otherwise = foldl1 (+) (map (^2) [1..n])
--right
sumsq5 :: Int -> Int
sumsq5 n
    | n <= 0 = 0
    | otherwise = foldr1 (+) (map (^2) [1..n ])



--2.1.3: The towers of Hanoi
hanoi :: Int -> Int
hanoi 0 = 0
hanoi n = 1 + 2 * hanoi (n-1)
--for correctness: avoid negative int
hanoi1 :: Int -> Int
hanoi1 n 
    | n <= 0 = 0
    | otherwise = 1 + 2 * hanoi(n-1)



--2.1.4: Factors
--nextFactor k n which returns smallest factor of n > k
nextFactor :: Int -> Int -> Int
nextFactor k n 
    | k >= n = n
    | mod n (k+1) == 0 = k+1
    | otherwise = nextFactor (k+1) n

smallestFactor :: Int -> Int
smallestFactor = nextFactor 1

-- computes the number of factors of n in the range 1..n, possibly except 1
numFactors :: Int -> Int
numFactors n = length [k | k <- [2..n], mod n k == 0]



--2.1.5: Defining types
type Month = Integer
--computes the number of days in a month, given also the year 
daysInMonth :: Month -> Integer -> Integer
daysInMonth m y 
    | m <= 0 || m > 12 = 0
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | m == 4 || m == 6 || m == 9 || m == 11 = 30
    | mod y 4 == 0 = 29
    | otherwise = 28

data Date = Date Integer Month Integer
--returns True if the day in the date lies between 1 and the number of days in the month
validDate :: Date -> Bool
validDate (Date y m d) 
    | (d > 0) && (d <= daysInMonth m y) = True
    | otherwise = False




--2.2 Lists

--2.2.1: Multiplting List Elements
--multiplies together all the elements of a list
multiply :: Num a => [a] -> a
multiply [] = 0
multiply xs = foldl1 (*) xs



--2.2.2: Substitution (Chakravarty)
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute old new (x:xs)
    | x == old = new:(substitute old new xs)
    | otherwise = x: (substitute old new xs)



--2.2.3: Avoiding duplicates 
