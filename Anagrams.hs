import Prelude
import System.Environment (getArgs)
import Data.Char (toLower)
import Data.Map (Map)
import Data.List (nub)
import qualified Data.Map as Map

-- takes a word and returns its character counts
-- list comphrension generates tuple such as ('a', 1) ('b', 1) ('a', 1) ('c', 1)
-- and fromListWith aggregates ones with the same key using + operation
wordCharCount :: [Char] -> Map Char Int
wordCharCount word = Map.fromListWith (+) [(c, 1) | c <- map toLower word]

-- takes a list and returns its character counts
-- basically same with the above, only difference is "concat sentence"
sentenceCharCount :: [[Char]] -> Map Char Int
sentenceCharCount sentence = Map.fromListWith (+) [(c, 1) | c <- map toLower $ concat sentence]

-- takes a list of dictionary words and returns a mapping from words to their character counts
-- generates a list of char2count maps for each word using wordCharCount along with the wordCharCount function
-- Map.fromList composes a map using output of list comphrension
dictCharCount :: [[Char]] -> Map [Char] (Map Char Int)
dictCharCount wordlist = Map.fromList[(word, wordCharCount word) | word <- wordlist]

-- takes in the result of the dictCharCount and returns a map of words 
-- which have the same character counts
-- first "map map (\a -> (snd a,[fst a]))"  switch keys and values, and new values converted into single element list
-- next  "Map.fromListWith (\a b -> a ++ b)" aggregates tuples with the same value using ++ operation
dictWordsByCharCount :: Map [Char] (Map Char Int) -> Map (Map Char Int) [[Char]]
dictWordsByCharCount wordCharCountPairs = Map.fromListWith (\a b -> a ++ b) $ map (\a -> (snd a,[fst a])) $ Map.toList wordCharCountPairs

-- which takes a word and the result of the previous function and returns a list of anagrams for the given word
-- Simple lookup, it can be either Nothing or Just a value. Return empty list incase of no match 
-- Actually, it can be handle with  Map.findWithDefault, see subtractCounts
wordAnagrams :: Map (Map Char Int) [[Char]] -> [Char] -> [[Char]]
wordAnagrams count2words word = case Map.lookup (wordCharCount word) count2words of
                                    Just value -> value
                                    _ -> []

-- takes character counts and returns all possible subsets of these counts
charCountsSubsets :: Map Char Int -> Map [Char] (Map Char Int)
charCountsSubsets char2count = dictCharCount $ nub  (subsets flattenCharCounts)
        where
            -- generate all subsets of a given list of chars
            -- recursively find subsets with x and without x
            -- and concats all of them. May generate duplicate sets so I applied nub.
            subsets :: [Char] -> [[Char]]
            subsets [] = [[]]
            subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

            -- converts char2count (e.g. [("a", 2), ("b", 1)]) to aab
            flattenCharCounts :: [Char]
            flattenCharCounts = concat [replicate (snd pair) (fst pair) | pair <- Map.toList char2count]

-- substracts given two char counts. Second assumed to be subset of first.
-- 1. We iterate over all of the chars of the first input
-- if char exits in the second one we substract the value
-- else Map returns a zero and zero gets substract and nothing changes
-- finally we filter all zero values
subtractCounts :: Map Char Int -> Map Char Int -> Map Char Int
subtractCounts char2count1 char2count2 = Map.fromList  $ filter (\a -> snd a /= 0) [sub tup1  (Map.findWithDefault 0 (fst tup1) char2count2)  | tup1 <- Map.toList char2count1]
    where
        sub tup1' value = (fst tup1',  (snd tup1') - value)


sentenceAnagrams :: [[Char]] -> Map (Map Char Int) [[Char]] -> [[[Char]]]
sentenceAnagrams sentence dictionaryCC = do
    helper sentenceCC
    where
        -- convert to lower case and filter non letter chars of given sentence and compute char counts 
        sentenceCC = sentenceCharCount [filter (\x -> x `elem` ['a' .. 'z']) $ map toLower word | word <- sentence]
        -- locally defined recursive function that does all of the job
        helper :: Map Char Int -> [[[Char]]]
        helper sentenceCC'  = case Map.toList sentenceCC' of
            [] -> [[]]  -- base case return an empty list
            -- This is a big list comphrension. It has the following structure
            -- for a in as
            --   for b in a
            --      if (...)
            --          map b recurse
            _ -> concat [
                            -- concat find word to list of words output by the recursive call
                            -- for example
                            -- let word = i, then recursive call will output "love you" and "you love"
                            -- then result will be "i love you"  "i you love" (actually there are many othert combinations)
                            -- and these are in a list format not space seperated
                            map (word : ) (helper (subtractCounts sentenceCC' (snd set))) |
                            -- itearete  over all subsets of givenchar sentence charCounts
                            set <- (Map.toList $ charCountsSubsets sentenceCC'),
                            -- itearete  over all matched anagram words exist in the dictionary
                            word <- Map.findWithDefault [] (snd set) dictionaryCC,
                            -- if word is not empty (there isanagram for the given set of char counts)
                            (length word) > 0
                        ]
           
main = do 
    --read the args
    args <- getArgs
    -- get the first arg (sentence) and split it using "words"
    let sentence = words $ head args
    -- read the dictionary
    fileContent <- readFile "words.txt"
    -- parse, convert to lower case and filter non letter chars
    let dict = [filter (\x -> x `elem` ['a' .. 'z']) $ map toLower word |word <- lines fileContent] ++ ["i"]
    -- compute dictionary char counts
    let dictionaryCharCounts = dictCharCount dict
    let dictionaryCC = dictWordsByCharCount dictionaryCharCounts
    -- mapM: monadic version of map. Print all anagrams in a nice format
    mapM print (map unwords $ sentenceAnagrams sentence dictionaryCC)
