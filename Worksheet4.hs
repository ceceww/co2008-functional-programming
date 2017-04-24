
-- CO 2008 Functional Programming 
-- Created: March 2017, University of Leicester, UK 
-------------------------------------------------------------------- 
--Student Name   Cecelia Wisniewska
-- Student Number 159021394
--------------------------------------------------------------------
--
-- Call this Worksheet5.hs when you submit it to the handin system!
--

module Worksheet4 where 

---------------------------------------------------------------------
----- EXERCISE 1
---------------------------------------------------------------------
data Value = A|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|J|Q|K
            deriving (Eq, Ord, Enum)
--- Part a)
instance Show Value where 
    show A = "A"
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show J = "J"
    show Q = "Q"
    show K = "K"

data Suite = Hearts | Spades | Clubs | Diamonds
             deriving (Eq, Ord, Enum)

--- Part b)
instance Show Suite where 
    show Hearts = "H"
    show Spades = "S"
    show Clubs = "C"
    show Diamonds = "D"

data Colour = Red | Black
              deriving (Eq, Ord,Enum, Show)

data Error a = Fail|Ok a
               deriving (Eq, Ord, Show)



type Card  = (Value, Suite)

--- Part c)

pack :: [Card]
pack = [(v,s) | v <- [A .. K], s <- [Hearts .. Diamonds]]


--- Part d)

colour :: Card -> Colour
colour c
    | snd c == Diamonds = Red
    | snd c == Hearts = Red
    | otherwise = Black

--- Part e)

split :: Int -> [a] -> (Error ([a],[a]))
split n list
    | (n <= length list && n>=0) = Ok (head(zip [take n list] [drop n list]))
    | otherwise = Fail

interleave ::  [a] ->  [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x:y:(interleave xs ys)

--- Part f)

shuffle :: [Int] -> [a] -> Error [a]
shuffle [] ys = Ok ys
shuffle (x:xs) ys = case (split x ys) of
                    Ok spl -> shuffle xs (interleave (fst(spl)) (snd(spl)))
                    Fail -> Fail

---------------------------------------------------------------------
----- EXERCISE 2
---------------------------------------------------------------------


data Btree a = ND | Data a |  Branch (Btree a) (Btree a)
               deriving (Show,Eq)

data Dir = L | R 
           deriving (Show,Eq)

type Path =  [Dir] 

type Breadcrumbs = [Dir] --reversed path
    
--- Part a)

extract :: Path -> Btree a -> Error a
extract [] (Data a) = Ok (a)
extract [L] (Branch (Data a) rtree) = Ok (a)
extract [R] (Branch ltree (Data a)) = Ok (a)
extract (L:p) (Branch ltree rtree) = extract p ltree
extract (R:p) (Branch ltree rtree) = extract p rtree
extract _ _ = Fail

--- Part b) 

add :: a -> Path -> Btree a -> Error (Btree a)
add a [] ND = Ok (Data a)
add a [L] (Branch ND rtree) = Ok (Branch (Data a) rtree)
add a [R] (Branch ltree ND) = Ok (Branch ltree (Data a))
add a (L:dirs) (Branch ltree rtree) = case (add a dirs ltree) of 
                                        Ok tree -> Ok (Branch tree rtree)
                                        Fail -> case (if (dirs /= []) then (add a (tail (dirs)) rtree) else Fail) of
                                                Ok tree -> Ok (Branch ltree tree)
                                                Fail -> Fail
add a (R:dirs) (Branch ltree rtree) = case (add a dirs rtree) of 
                                        Ok tree -> Ok (Branch ltree tree)
                                        Fail -> case (if (dirs /= []) then (add a (tail (dirs)) ltree) else Fail) of
                                                Ok tree -> Ok (Branch tree rtree)
                                                Fail -> Fail
add _ _ _ = Fail

--- Part c)

findpath :: Eq b => (a -> b) -> b -> Btree a -> [Path]
findpath f x ND = []
findpath f x (Data a)
   | f a == x = [[]]
   | f a /= x = []
findpath f x (Branch (Data a) rt)
   | f a == x = [L] : findpath f x rt
   | f a /= x = findpath f x rt
findpath f x (Branch lt (Data a))
   | f a == x = [R] : findpath f x lt
   | f a /= x = findpath f x lt
findpath f x (Branch lt rt) = findpath f x lt ++ findpath f x rt

tree1 = Branch ND ND
tree2 = Branch ND (Data 3)
tree3 = Branch tree1 tree2
tree4 = Branch (Data 3) (Data 4)
tree5 = Branch tree3 tree4


---------------------------------------------------------------------
----- EXERCISE 3 Family tree question
---------------------------------------------------------------------



-- a

{-- write here your answer
The phrase "deriving Show" is added at the end of the data declaration for Tree so Haskell 
automatically makes it part of the Show typeclass. Show is for converting values of Tree to String.
The declaration of person does not need to derive Show because Person is of type String.
--}

-- b
{-
putStr :: String -> IO ()
Output will be: Hello
What putStr does is write a string to the standard output device.
--}

-- c

sort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sort ord [] = []
sort ord(x:xs) = sort ord less ++ occs ++ sort ord more
  where less = [ e | e <- xs, ord e x]
        occs = x : [ e | e <- xs, e == x]
        more = [ e | e <- xs, ord x e]

-- d
data Tree a = U | F a (Tree a) (Tree a) deriving Show

term = F "Anna" (F "Fer-Jan" (F "Willem" U U) (F "Nettie" U U)) (F "Paula" (F "Mario" U U) (F "Martha" U U))
type Person = String

--the function genlabel below is slightly different from the one specified in the worksheet:
--it also has an input integer n which should be the start (root) label of 1 to generate the required output
genlabel :: Int -> (Tree Person) -> (Tree (Int,Person))
genlabel n (F person U U) = F (n, person) U U
genlabel n (F person lt U) = (F (n, person) (genlabel (2*n) lt) U)
genlabel n (F person U rt) = (F (n, person) U (genlabel (2*n+1) rt))
genlabel n (F person lt rt) = (F (n, person) (genlabel (2*n) lt) (genlabel (2*n+1) rt))

--e
preprint ::  (Int,Person) -> String
preprint (n, person) = show n ++ ": " ++ person 


type Tile = [String]

ext2 :: Tree (Int, Person) -> [(Int, Person)]
ext2 (F (n,person) U U) = [(n, person)]
ext2 (F (n, person) lt rt) = [(n,person)] ++ ext2 lt ++ ext2 rt
 

ext :: Tree (Person) -> [(Int, Person)]
ext tree = ext2 (genlabel 1 tree)

mapP :: Tree Person -> Tile
mapP tree = map (preprint) (sort (<) (ext tree))

printElements :: [String] -> IO()
printElements = mapM_ putStrLn

--flatten :: Tile -> String
--flatten xs = 


printlist :: Tree Person -> IO()
printlist tree = printElements (mapP tree)



tree = F "Anna" (F "Fer-Jan" (F "Willem" U U) (F "Nettie" U U)) (F "Paula" (F "Mario" U U) (F "Martha" U U))


--test1 = printlist tree

--f

--print2Dtree :: Tree Person -> IO()

--test2 = print2Dtree tree



--Don't forget your name

-- please take care that your solution compiles.
-- of course if things don't work, you can comment them out
-- and explain in the comment that that something is wrong with it.
