import Prelude

-- Maps numeric value to corresponding day
num2day v = case v of
    0 -> "Saturday"
    1 -> "Sunday"
    2 -> "Monday"
    3 -> "Tuesday"
    4 -> "Wednesday" 
    5 -> "Thursday"
    6 -> "Friday"
    _ -> "Unknown day" 
{-
    Implementation of Zeller's congruence
    I used "div" for division, div on two integer values corresponds to integer division 
    like Python's // so I did not need explicit floor or toInteger operations
-}
dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = if m <= 2 then
                    dayOfWeek (y - 1) (m + 12) d
                  else
                      (d + t1 + k + t2 + t3 + (5 * j)) `mod` 7
                        where
                            j  = div y 100 
                            k  = mod y 100
                            t1 = div (13 * (m + 1))  5
                            t2 = div k 4
                            t3 = div j 4

-- Filled version of given template
sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1 -- Date: start january first
    where 
        sundays' :: Integer -> Integer -> Integer
        sundays' y m 
            | y > end = 0 -- check if we reach the final year
            | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest -- +1 if its Sunday and check next month
            where
                nextY = if m == 12 then y + 1 else y -- increment if its the last month
                nextM = if m == 12 then 1 else m + 1 -- reset if its last month else increment
                rest = sundays' nextY nextM

-- Above function has a very common pattern. It is trivial to convert to a tail recursive function
-- Instead of performing +1 operations on the recursive call, I added an accumalator called acc
sundays1tr :: Integer -> Integer -> Integer
sundays1tr start end = sundays1tr' start 1 0
    where 
        sundays1tr' :: Integer -> Integer -> Integer -> Integer
        sundays1tr' y m acc -- Extra acc parameter
            | y > end = 0
            | otherwise = if dayOfWeek y m 1 == 1 then restCurried acc+1 else restCurried acc
            where
                nextY = if m == 12 then y + 1 else y 
                nextM = if m == 12 then 1 else m + 1 
                restCurried = sundays1tr' nextY nextM 
                -- since both calls have shared parameters all used currying.

-- Check whether a given year is a leap year or not
leap :: Integer -> Bool
leap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0) 

-- Get the number of days in a given month of a given year
dayInMonth :: Integer -> Integer -> Integer
dayInMonth m y = if m == 2 then if leap y then 29 else 28
                    else 
                        if m `elem` [4,6,9,11] then 30 else 31

-- Since we know that the 1st Jan 1900 was Monday, we can exploit this
-- fact to come up with a more efficient solution.
-- This is the Haskell version of given Python code.  
sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' start 1  (dayOfWeek start 1 1)
    where 
        sundays' :: Integer -> Integer -> Integer -> Integer
        sundays' y m weekday
            | y > end = 0
              -- This could be tail-rec but I left it like this since its not asked 
            | otherwise = call
            where
                call = if weekday `mod` 7 == 1 then rest + 1 else rest 
                nextY = if m == 12 then y + 1 else y 
                nextM = if m == 12 then 1 else m + 1 
                weekday' = weekday + (dayInMonth m y `mod` 7)
                rest = sundays' nextY nextM weekday'

getFunction :: String -> (Integer -> Integer -> Integer)
getFunction name
    | name == "sundays1" = sundays1
    | name == "sundays1tr" = sundays1tr
    | name == "sundays2" = sundays2
    | otherwise = error "unknown function"

main :: IO ()
main = do
    line <- getLine
    let [f, start, end] = words line
    putStrLn $ show $ (getFunction f) (read start :: Integer) (read end :: Integer)                
