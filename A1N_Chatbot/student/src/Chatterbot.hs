
-- NANCY TRUONG

module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)
import qualified Data.Either as E
import Data.Data (ConstrRep(FloatConstr))

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a = Wildcard | Item a
  deriving (Eq, Show)

-- A pattern is a list of pattern elements
newtype Pattern a = Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string, list of words
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
makePair (Rule (pat, templates)) = do
  r <- randomIO :: IO Float
  return (pat, pick r templates)
--makePair = undefined


-- try :: (a -> Maybe a) -> a -> a
-- try f x = maybe x id (f x)
-- i.e. if f x returns Just y, use y
  -- if f x fails with Nothing, keep original x
rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
rulesApply rules = try(transformationsApply reflect rules)
-- transformationsApply reflect rules :: Phrase -> Maybe Phrase

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b
-- map applies the function to every element in the list
-- map :: (a -> b) -> [a] -> [b]
reflect :: Phrase -> Phrase
reflect = map reflectWord 
  where reflectWord w = fromMaybe w (lookup w reflections)

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

-- remove punctuation characters with filter(...)
--
prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
ruleCompile (pat, templates) =
  Rule (starPattern (map toLower pat), map starPattern templates)
--ruleCompile = undefined

--------------------------------------


-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: Eq a => a -> [a] -> Pattern a
mkPattern wc = Pattern . map toPatternElem
  where 
    toPatternElem x git
      | x == wc = Wildcard
      | otherwise = Item x 
--mkPattern = undefined

stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words 
-- words :: String -> [String] (converting string to phrase - list of words)
-- then make pattern wc (wildCard) [String]

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


--fix :: Eq a => (a -> a) -> a -> a
--fix f x
--   |  f x == x  = x
--   |  otherwise = fix f (f x)
-- keep applying f until the result stops changing
reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
reductionsApply reductions = fix(try(transformationsApply id reductions))


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
-- ((mmap transform) . (match pat)) x is equivalent to mmap transform  (match pat x)
-- map f x so first match pat with [a]
-- if the results of match pat [a] is Nothing then Nothing
-- if rhe results of match pat [a] is Just x then Just (f x), meaning apply transform to that result
-- transform is a function from lists to lists

-- Applying a single pattern
-- first argumetn: ([a] -> [a]), is a function from lists to lists (e.g. id, reflect,...)
-- second argument: [a], is the input string/list we want to match
-- third argument: (Pattern a, Template a)
transformationApply :: Eq a => 
  ([a] -> [a]) -> -- function to apply to extracted match
  [a] -> -- string to match
  (Pattern a, Template a) -> -- the transformation
  Maybe [a] -- the result
transformationApply transform input (pat, template) = 
  mmap (substitute template) (matchAndTransform transform pat input)
-- i.e
-- case matchAndTransform transform pat input of
--    Nothing -> Nothing
--    Just xs -> Just (substitute template xs)


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => 
  ([a] -> [a]) -> -- transform function to apply to extracted match
  [(Pattern a, Template a)] -> -- list of transformations
  [a] -> -- string to match
  Maybe [a] -- result
transformationsApply _ [] _ = Nothing
transformationsApply transform (t:ts) input =
  transformationApply transform input t `orElse`
  transformationsApply transform ts input
--transformationsApply = undefined
