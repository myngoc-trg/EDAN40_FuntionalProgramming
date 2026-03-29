module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a = Wildcard | Item a
  deriving (Eq, Show)

-- A pattern is a list of pattern elements
newtype Pattern a = Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string
type Phrase = [String]

-- One rule contains: 
-- one pattern (a list of pattern elements, e.g. ["I", "need",Wildcard]) 
-- and one list of several possible templates
newtype Rule = Rule (Pattern String, [Template String])
  deriving (Eq, Show)

-- The whole chatbot brain is a list of rules
type BotBrain = [Rule]

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine

      -- stateOfMind brain randomly chooses one template from each rule 
      -- and gives back a function Phrase -> Phrase
      answer <- stateOfMind brain

      -- prepare: cleans the user input
      -- answer: applies the rules
      -- present: turns the phrase back to text
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()


--------------------------------------------------------

-- This takes a brain, and returns a function
-- Which will take a phrase as an input and calculate the result
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b =
  fmap rulesApply (mapM makePair b)

-- A rule maps a pattern to many answers, so we choose one
-- at random, and that's our bot
makePair :: Rule -> IO (Pattern String, Template String)
{- TO BE WRITTEN -}
makePair = undefined

rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply = undefined

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = undefined

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
{- TO BE WRITTEN -}
ruleCompile = undefined

--------------------------------------


-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: Eq a => a -> [a] -> Pattern a
mkPattern wc = Pattern . map toPatternElem
  where 
    toPatternElem x 
      | x == wc = Wildcard
      | otherwise = Item x 
--mkPattern = undefined

stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words

starPattern :: String -> Pattern String
starPattern = stringToPattern "*"

reductions :: [(Pattern String, Pattern String)]
reductions = (map . map2) (starPattern, starPattern)
  [ ( "please *", "*" ),
    ( "could you *", "*" ),
    ( "can you *", "*"),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply = undefined


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a template with the list given as the second argument
-- Takes a template (also a Pattern) and a replacement list, and returns a new list
substitute :: Eq a => Template a -> [a] -> [a]
substitute (Pattern []) _ = []
-- Recursive: If the template starts with a normal Item x, keep that item, then recursively substitute through the rest of the template
-- Item x : ps means if Patten [Item "why", Wildcard, Item "?"], then x = "why" and ps = [ Wildcard, Item "?"]
substitute (Pattern (Item x : ps)) rs = x : substitute (Pattern ps) rs
substitute (Pattern (Wildcard : ps)) rs = rs ++ substitute (Pattern ps) rs
--substitute = undefined

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
-- three possible cases:
-- 1. The pattern and the list can't match, so the function returns Nothing, indicating the match failed.
-- 2. The pattern and the list match, in which case there are two cases:
--      1. The pattern didn't contain a wildcard, in which case we extract nothing, the function returns Just [].
--      2. The pattern and the list match, with wildcards, so we extracted something from the list, in which
-- case the result with have the shape Just [...].
match :: Eq a => Pattern a -> [a] -> Maybe [a]
-- 2. The pattern and the list match, in which case there are two cases:
-- 2.1 The pattern didn't contain a wildcard, in which case we extract nothing, the function returns Just [].
-- If the pattern is empty and the list is empty, they match, and we didn't extract anything, so we return Just [].
match (Pattern []) [] = Just []
-- 1. The pattern and the list can't match, so the function returns Nothing, indicating the match failed.
-- If the pattern is empty but the list is not, they can't match. 
-- If the list is empty but the pattern is not, they can't match.
match (Pattern []) _ = Nothing
match _ [] = Nothing
-- If the pattern starts with a normal Item x, then the list must also start with x, and the rest of the pattern must match the rest of the list.
match (Pattern (Item x : ps)) (y:ys)
  | x == y = match (Pattern ps) ys
  | otherwise = Nothing

-- If the pattern starts with a wildcard, then we have two options: 
-- for example, Pattern [Wildcard, Item 'd', Item 'o'] which corresponds to "*do"
-- we can match the wildcard to a single item in the list, "bdo" => "b" + "do"
-- or we can match it to a longer sublist. We try both options, "dobedo" => "do" + "bedo"
-- and if either one succeeds, we return the result.
match pat@(Pattern (Wildcard:_)) xs =
  orElse (singleWildcardMatch pat xs) (longerWildcardMatch pat xs)
--match = undefined

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => Pattern a -> [a] -> Maybe [a]
-- First, we match the rest of the list with the pattern, if that worked, we return the current element
--, otherwise, Nothing.
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern ps) xs of
    Nothing -> Nothing
    Just _ -> Just [x]

-- it doesn't consume the first wildcard of the pattern.
-- it extracts the match, and prepends the current element of the list to it.
longerWildcardMatch pat@(Pattern (Wildcard:_)) (x:xs) =
  mmap (x:) (match pat xs) -- we try to match the pattern with the rest of the list, if that worked, we prepend the current element to the result, otherwise, Nothing.
longerWildcardMatch _ _ = Nothing 
--longerWildcardMatch = undefined



-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------

-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: Eq a => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat)

-- Applying a single pattern
transformationApply :: Eq a => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
{- TO BE WRITTEN -}
transformationApply = undefined

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
transformationsApply = undefined
