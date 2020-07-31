import System.IO
import System.Environment
import Prelude
import Data.Char (toLower)

-- Extra score attribute for ORD class
data Ninja = Ninja {name :: String, country :: Char,
                    status :: String, exam1 :: Float,
                    exam2 :: Float, ability1 :: String,
                    ability2 :: String, r :: Int, score:: Float}

-- implementing show class for the ninja         
instance Show Ninja where 
    show (Ninja name _ status _ _ _ _ r score) = name ++ ", " ++ "Score: " ++ (show score) ++ " Status: " ++ status ++ ", Round: " ++ (show r)

-- implementing Eq class for the ninja, required for ORD class, just check the names   
instance Eq Ninja where
    (==) ninja1  ninja2 = name ninja1 == name ninja2

-- implementing ORD class for ninja, used by sortBy function
instance Ord Ninja where 
    compare ninja1  ninja2
        | r ninja1 == r ninja2 = case () of
            ()| score ninja1  <= score ninja2  -> GT
              | otherwise -> LT
        | r ninja1 <= r ninja2 = LT
        | otherwise = GT

-- Round operation, returns True if fighter one is the winner else false.
-- If everthing is equal fighter one wins 
(<?>) :: Ninja -> Ninja -> Bool
(<?>) ninja1 ninja2 
        | score ninja1 > score ninja2 = True
        | score ninja1 < score ninja2 = False
        | otherwise = case () of
                ()| a11 + a12 > a21 + a22 -> True
                  | a11 + a12 < a21 + a22  -> False
                  | otherwise -> True
                    where
                        a11 = (abilityTable $ ability1 ninja1)
                        a12 = (abilityTable $ ability2 ninja1)
                        a21 = (abilityTable $ ability1 ninja2)
                        a22 = (abilityTable $ ability2 ninja2)

-- map keystroke to country, return an empty list and error message incase of a typo 
getCountryInfo list country 
            | country `elem` ["e", "E"] = ((list !! 0), "Earth")
            | country `elem` ["l", "L"] = ((list !! 1), "Lightning")
            | country `elem` ["w", "W"] = ((list !! 2), "Water")
            | country `elem` ["n", "N"] = ((list !! 3), "Wind")
            | country `elem` ["f", "F"] = ((list !! 4), "Fire")
            |otherwise = ([], "Unknown country code")

-- check country list, if anyone is promoted to Journeyman return true
checkJourneyMan list  = length (filter (\x -> status x == "Journeyman") list) == 1

-- option (d) from menu
roundBetweenCountries :: [[Ninja]] -> IO ()
roundBetweenCountries allNinjas =
    do 
        putStr "Enter the first country code: " >> hFlush stdout
        ninjaCountry1 <- getLine
        let getCountryInfoCurried = getCountryInfo  allNinjas 
        let (list1, country1) = getCountryInfoCurried ninjaCountry1
        if null list1 
            then
                putStr (if country1 == "Unknown country code" then "Unknown country code\n" else "No available ninja\n") >> roundBetweenCountries allNinjas 
            else do
                if checkJourneyMan list1 
                    then putStrLn (country1 ++ " country cannot be included in a fight" ++ "\n") >> roundBetweenCountries allNinjas
                    else  do
                        putStr "Enter the second country code: " >> hFlush stdout
                        ninjaCountry2 <- getLine
                        let (list2, country2) = getCountryInfoCurried  ninjaCountry2
                        if null list2 
                            then
                                putStr (if country2 == "Unknown country code" then "Unknown country code\n" else "No available ninja\n") >> roundBetweenCountries allNinjas
                            else do
                                if country1 == country2 
                                    then 
                                        putStr "\nOption (d) is for a round between two distinct countries.\nSelect (c) and enter ninjas manually or input two different countries.\n\n" >>  menu allNinjas
                                    else do
                                        if checkJourneyMan list2 
                                            then putStrLn (country2 ++ " country cannot be included in a fight" ++ "\n") >> roundBetweenCountries allNinjas
                                            else do
                                                let ninja1 = head $ sortBy list1
                                                let ninja2 = head $ sortBy list2
                                                let res = ninja1 <?> ninja2
                                                let (newList,printWinner) = updateList res list1 list2  ninja1 ninja2 allNinjas
                                                printWinner
                                                menu newList

-- option (e) from menu
exit :: [[Ninja]] -> IO ()
exit list = mapM_ putStrLn . printList . sortBy $ filter (\x -> status x == "Journeyman") [ninja | ninjaList <- list,  ninja <- ninjaList]

-- option (a) from menu
-- fills the list of given country, sorts the list and prints it (if there is Journeyman, it gives warning)
aCountryNinjaInfo :: [[Ninja]] -> IO ()
aCountryNinjaInfo list = do
    putStr "Enter the country code: "
    hFlush stdout
    country <- getLine
    let (l, countryName) = getCountryInfo  list country
    let warning = if checkJourneyMan l then countryName ++ " country cannot be included in a fight" ++ "\n" else ""
    if null l then putStrLn  (if countryName == "Unknown country code" then "Unknown country code\n" else "No available ninja\n") else  mapM_ putStrLn . printList $ sortBy l
    putStrLn warning
    menu list

-- option (b) from menu
-- prints all ninjas from five countries
allCountriesNinjaInfo ::[[Ninja]] -> IO ()
allCountriesNinjaInfo list = do
    let allCountries = (list !! 0) ++ (list !! 1) ++ (list !! 2) ++ (list !! 3) ++ (list !! 4)
    mapM_ putStrLn . printList $ sortBy allCountries
    putStrLn ""
    menu list   


-- option (c) from menu
-- function takes necessary inputs from the user, makes a fight and updates the list based on the fight result
roundBetweenNinjas :: [[Ninja]] -> IO ()
roundBetweenNinjas list  = do

    putStr "Enter the name of the first ninja: " >> hFlush stdout
    ninjaName1 <- getLine
    putStr "Enter the country code of the first ninja: " >> hFlush stdout
    ninjaCountry1 <- getLine
    let getCountryInfoCurried = getCountryInfo  list 

    let (list1, country1) = getCountryInfoCurried ninjaCountry1
    if checkJourneyMan list1 
        then putStrLn (country1 ++ " country cannot be included in a fight" ++ "\n") >> roundBetweenNinjas list
        else  do
            if null list1 then putStrLn "Unknown country code" >> roundBetweenNinjas list
                else do
                    let ninja1 = filter (\x' -> (map toLower  $ name x') == (map toLower ninjaName1)) list1
                    if null ninja1 then putStrLn "There is no such ninja" >> roundBetweenNinjas list
                    else do
                        putStr "Enter the name of the second ninja: " >> hFlush stdout
                        ninjaName2 <- getLine
                        putStr "Enter the country code of the second ninja: " >> hFlush stdout
                        ninjaCountry2 <- getLine

                        let (list2, country2) = getCountryInfoCurried ninjaCountry2
                        if checkJourneyMan list2 
                            then putStrLn (country2 ++ " country cannot be included in a fight" ++ "\n") >> roundBetweenNinjas list
                            else  do
                                if null list2 then putStrLn "Unknown country code \n" >> roundBetweenNinjas list
                                else do
                                    let ninja2 = filter (\x' -> (map toLower  $ name x') == (map toLower ninjaName2)) list2
                                    if null ninja2 then putStrLn "There is no such ninja" >> roundBetweenNinjas list 
                                    else do
                                        if ninja1 == ninja2 && country1 == country2 then putStrLn "Please input two different ninjas" >> roundBetweenNinjas list
                                        else do 
                                            let (newList,printWinner) = (updateList ((<?>) (head ninja1)  (head ninja2)) list1 list2 (head ninja1) (head ninja2) list)
                                            printWinner
                                            menu newList

-- function returns updated list (winner ninja will be updated and defeated ninja will be removed)
-- also function prints the winner ninja
-- also this function checks whether if two ninjas are from the same country
updateList :: Bool -> [Ninja] -> [Ninja] -> Ninja -> Ninja -> [[Ninja]] -> ([[Ninja]], IO())
updateList result list1 list2 ninja1 ninja2 list
    | result == True    = (update2 (if (country ninja1 == country ninja2) then snd res1 else list2) ninja2 (fst res1), printWinner $ updateNinja ninja1)
    | otherwise         = (update2 (if (country ninja1 == country ninja2) then snd res2 else list1) ninja1 (fst res2), printWinner $ updateNinja ninja2)
    where
        update1 l1 n1 = replace' l1 ((filter (/=n1) l1) ++ [updateNinja n1]) list
        update2 l2 n2 list' = fst (replace' l2 (filter (/=n2) l2) list')
        updateNinja n1' = (if r n1' + 1 == 3 then n1'{score = score n1' + 10, r = 3, status = "Journeyman"}
                        else n1'{score = score n1' + 10, r = r n1' + 1})
        printWinner n1' = putStrLn ("Winner: \"" ++ name n1' ++ ", Round: " ++ show (r n1') ++ ", Status: " ++ status n1' ++ "\"\n")
        res1 = (update1 list1 ninja1)
        res2 = (update1 list2 ninja2)


-- function is used to replace an element ([Ninja]) with the new updated element from our list ([[Ninja]])
replace' :: Eq t => t -> t -> [t] -> ([t], t)
replace' a b list= (map (\x -> if (a == x) then b else x) list, b)

--menu is displayed to user
menu :: [[Ninja]] -> IO ()
menu list = do
    putStrLn "a) View a Country's Ninja Information"
    putStrLn "b) View All Countries' Ninja Information"
    putStrLn "c) Make a Round Between Ninjas"
    putStrLn "d) Make a Round Between Countries"
    putStrLn "e) Exit"
    putStr "Enter the action: "
    hFlush stdout
    choice <- getLine
    if choice `elem` ["a", "A", "b", "B", "c", "C", "d", "D", "e", "E"] then  callTheFunction list choice else do  putStrLn "Unknown option \n"  >> menu list  


-- menu calls the proper function based on the user selection
callTheFunction :: [[Ninja]] -> String -> IO()
callTheFunction list choice'
  | choice' `elem` ["a", "A"] = aCountryNinjaInfo list
  | choice' `elem` ["b", "B"] = allCountriesNinjaInfo list
  | choice' `elem` ["c", "C"] = roundBetweenNinjas list
  | choice' `elem` ["d", "D"] = roundBetweenCountries list
  | choice' `elem` ["e", "E"] = exit list
  | otherwise = putStrLn "Unknown option"         


-- After reading file, list contains file content line by line, this function is called to split these lines to features
splitFeatures :: [[Char]] -> [[[Char]]]
splitFeatures list@(l:ls) = map tokenize list
    where 
        tokenize :: [Char] -> [[Char]]
        tokenize l' = case break (==' ') l' of
            (newList', "") -> [newList']
            (newList', ll:lls) -> newList' : tokenize lls


-- function creates a list that contains all ninjas from five countries (list contains lists of countries)
-- output will be like [earthList, lightningList, waterList, windList, fireList]
-- this list will be transferred between menu options
allCountryLists :: [[[Char]]] -> [[Ninja]]
allCountryLists list =  (fillList list "Earth") : (fillList list "Lightning")
                            : (fillList list "Water") :  (fillList list "Wind")
                            : (fillList list "Fire") : []


-- function takes an element from list (list contains all ninjas' features like [[f11, f12,..], [f21, f22, ..], ...])
-- and creates ninja objects then gives a list that contains these objects
fillList :: [[[Char]]] -> [Char] -> [Ninja]
fillList newList country = [newNinja x | x <- list]
    where
        list = filter (\x' -> (x' !! 1) == country) newList
        newNinja x = Ninja {name = (x !! 0) , country = (if x !! 1 == "Wind" then 'n' else head (x !! 1)), status = "Junior" , exam1 = e1, 
                    exam2 = e2, ability1 = a1, ability2 = a2, r = 0,
                    score = (calculateScore e1 e2 (abilityTable a1) (abilityTable a2))}
                        where
                            e1 = (read (x !! 2) :: Float)
                            e2 = (read (x !! 3) :: Float)
                            a1 = (x !! 4)
                            a2 = (x !! 5)

-- ability impacts
abilityTable :: String -> Float
abilityTable ability = case ability of
    "Clone" -> 20.0
    "Hit" -> 10.0
    "Lightning" -> 50.0
    "Vision" -> 30.0
    "Sand" -> 50.0
    "Fire" -> 40.0
    "Water" -> 30.0
    "Blade" -> 20.0
    "Summon" -> 50.0
    "Storm" -> 10.0
    "Rock" -> 20.0
    _ -> 0.0

-- print a country list with desired format (string)
printList :: [Ninja] -> [String]
printList list = [show x | x <- list]
    
-- calculate ninja score based on exams and abilities points
calculateScore :: Float -> Float -> Float -> Float -> Float
calculateScore e1 e2 a1 a2 = 0.5 * e1 + 0.3 * e2 + a1 + a2

-- quick sort is used to sort country lists
sortBy :: [Ninja] -> [Ninja]
sortBy [] = []
sortBy (x:xs) = sortBy small ++ [x] ++ sortBy large
    where
        small = [a | a <- xs, a <= x]
        large = [b | b <- xs, b > x]


main = do 
    --takes file name from command line
    fileName <- getArgs

    -- read file
    fileContent <- readFile $ head fileName

    -- create a list from file content 
    --(each element is a line, split lines to features, create ninja objects and get a list that contains country lists)
    let list = allCountryLists $ splitFeatures $ lines fileContent 
    menu list
