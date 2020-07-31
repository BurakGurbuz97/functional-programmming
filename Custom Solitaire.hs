import Prelude
import Data.Char (isDigit, digitToInt)
-- Due to "raise error" term in the take home exam sheet
-- I used error "..." instead of putStrLen "..."

-- Given definition
data Color = Red | Black deriving(Eq, Show)
data Suit = Clubs | Diamonds | Hearts | Spades deriving(Eq, Show)
data Rank = Num Int | Jack | Queen | King | Ace deriving(Eq, Show)
data Card = Card {suit :: Suit, rank :: Rank} deriving(Eq, Show)
data Move = Draw | Discard Card deriving(Eq, Show)

-- Takes a card and returns its color (spades and clubs are black, diamonds and hearts are red).
cardColor :: Card -> Color
cardColor c = case suit c of
    Spades -> Black
    Clubs -> Black
    Diamonds -> Red
    _ -> Red

-- Takes a card and returns its value (numbered cards have their number as the value, aces are 11, everything else is 10)
cardValue ::  Card -> Int
cardValue c = case rank c of
    Num x -> x
    Ace -> 11
    _ -> 10

-- Takes a list of cards cs, and a card c. It returns a list that has all the elements of cs except c
removeCard :: [Card] -> Card -> [Card]
removeCard cs c = case break (c==) cs of --splits the list into tuple as (beforeConditionElems, Condition:AfterConditionElems)
    (_, []) ->  error "card not in list"
    ([], x:xs) -> xs -- first card is to be discarded
    (xs1, x2:xs2) -> xs1 ++ xs2 -- any other possibility is covered here

-- True if all the cards in the list are the same color and False otherwise.
allSameColor :: [Card] -> Bool
allSameColor cards = case cards  of
    [] -> True -- true for empty list
    [c] -> True
    _ -> if (all (==Black) $ map (cardColor) cards) || (all (==Red) $ map (cardColor) cards) then True else False


-- Takes a list of cards and returns the sum of their values. 
sumCards :: [Card] -> Int
sumCards [] = 0
sumCards (c:cs) =  sumCards' cs $ cardValue c
    where
        sumCards' [] acc = acc -- return accumulator
        sumCards' (c':cs') acc = sumCards' cs' $ acc + cardValue c' -- recursive call

-- Computes the score as described.
score :: [Card] -> Int -> Int
score cards goal
    -- exceeds the goal
    | sumScore > goal = if sameColor then 3*(sumScore - goal) `div` 2 else  3*(sumScore - goal)
    -- below the goal
    | sumScore < goal = if sameColor then div (goal - sumScore)  2  else (goal - sumScore)
    -- hits the bull's-eye
    | otherwise = 0
        where 
            sumScore = sumCards cards
            sameColor = allSameColor cards

-- the state of the game, list of cards and heldcards.
data Game = Game {cardList :: [Card] , heldCards :: [Card]}


-- takes a card list, move list, and goal. Returns the score at the end of the game. 
runGame :: [Card] -> [Move] -> Int -> Int
runGame cards moves g = runGame' moves (Game {cardList = cards, heldCards = []})
    where
        -- Locally defined helper function
        runGame' :: [Move] -> Game -> Int
        runGame' [] state' = score (heldCards state')  g -- No moves left
        runGame' (m:ms) state' = if sumCards (heldCards state') > g 
                                        then  runGame' [] state'
                                        else
                                            case m of
                                                -- Discard a card from hand
                                                Discard c -> runGame' ms  Game {cardList = cardList state',
                                                                        heldCards = removeCard (heldCards state') c}
                                                -- Draw a card
                                                _ -> if (length $ cardList state') == 0 then runGame' [] state' else runGame' ms  Game {cardList = tail (cardList state'),
                                                                        heldCards = head (cardList state') : (heldCards state')}

-- Takes a character c and returns the corresponding suit. 
convertSuit :: Char -> Suit
convertSuit c 
    | c == 'C' || c == 'c' = Clubs
    | c == 'D' || c == 'd' = Diamonds  
    | c == 'H' || c == 'h' = Hearts  
    | c == 'S' || c == 's' = Spades  
    | otherwise = error  "suit is unknown"

-- Takes a character c and returns the corresponding rank
convertRank :: Char -> Rank
convertRank c
    | isDigit c == True = case  c of
                            '1' -> Ace
                            _  ->  Num (digitToInt  c)
    | otherwise = case c of 
                    't' -> Num 10
                    'T' -> Num 10
                    'j' -> Jack
                    'J' -> Jack
                    'q' -> Queen
                    'Q' -> Queen
                    'k' -> King
                    'K' -> King
                    _ -> error "rank is unknown"

-- Takes a suit name (char) and a rank name (char), and returns a card
convertCard :: Char -> Char -> Card
convertCard s r = Card {suit = convertSuit s, rank = convertRank r}

-- Read (and return) a list of cards from the user
readCards :: IO [Card]
readCards = readCard' []
           where
               readCard' :: [Card] ->  IO [Card]
               readCard' cards = do line <- getLine
                                    if line == "."
                                        then return cards -- Input for cards are ended
                                        else
                                            -- check if the input line consist of 2 chars like c1 or s1.
                                            if  (length line == 2) then readCard' (cards ++ [convertCard (line !! 0)  (line !! 1)])
                                            -- input is malformed
                                            else
                                                error "Lenght of card input is wrong"

-- takes a move name (char), a suit name (char), and a rank name (char) and returns a move
convertMove :: [Char] -> Move
convertMove line
    | line !! 0  == 'd' || line !! 0  == 'D' = Draw 
    | line !! 0  == 'r' || line !! 0  == 'R' = Discard (convertCard (line !! 1)  (line !! 2))
    | otherwise = error "move is unknown"
                
    
-- read (and return) a list of moves from the user
readMoves :: IO [Move]
readMoves = readMove' []
           where
               readMove' :: [Move] ->  IO [Move]
               readMove' moves = do line <- getLine
                                    if line == "."
                                        then return moves -- Input for cards are ended
                                        else
                                            -- check if the input line consist of 3 or 1 chars like rhj or d.
                                            if  (length line == 3 || length line == 1) then readMove' (moves ++ [convertMove line])
                                            else
                                                error "Lenght of move input is wrong"


-- Bring all of it together 
main = do 
        putStrLn "Enter cards:"
        cards <- readCards

        putStrLn "Enter moves:"
        moves <- readMoves

        putStrLn "Enter goal:"
        line <- getLine

        let goal = read line :: Int
        let score = runGame cards moves goal
        putStrLn ("Score: " ++ show score)
