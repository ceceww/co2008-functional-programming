
--------------------------------------------------------------------
-- CO 2008  Functional Programming
-- Created: February 2017, University of Leicester, UK
-- handin 15.00 hr on 14th February (mark will count towards module outcome) 
--------------------------------------------------------------------
-- Student Name: Cecelia Wisniewska
-- Student Number: 159021394
--------------------------------------------------------------------
--
-- Please don't hand in buggy solutions. That makes the marking harder.
-- Points may be deducted if your solution does not compile properly...
--


module Worksheet2 where
import Data.Char

----------------------------------------------------------------------
-- Exercise 1: A phone book
---------------------------------------------------------------------

type Name = String
type PhoneNumber = Int
type Person  = (Name, PhoneNumber)
type PhoneBook = [Person]



-- Part a)

add :: Person -> PhoneBook -> PhoneBook
add person phonebook = person : phonebook


-- Part b)

delete  :: Name -> PhoneBook -> PhoneBook
delete name phonebook = filter ((/=name).fst) phonebook


--  Part c)

find  :: Name -> PhoneBook -> [PhoneNumber]
find name phonebook = [ snd(x) | x <- phonebook, fst(x) == name]

--  Part d)

update :: Name ->  PhoneNumber ->  PhoneNumber->   PhoneBook ->PhoneBook
update name oldnumber newnumber phonebook = map (\ x -> if x == (name, oldnumber) then (name, newnumber) else x) phonebook

-----------------------------------------------------------------
-- Exercise 2:  Customers of a Bank
-----------------------------------------------------------------

type NI = Int
type Age = Int
type Balance = Float
type Customer  = (NI,Age, Balance)
type Bank =  [Customer]

getage (a,b,c) = b
getNI (a,b,c) = a 
getbalance (a,b,c) = c

-- Part a)
retired :: Customer -> Bool
retired customer = if getage customer >= 60 then True else False

-- Part b)
deposit :: Customer -> Float -> Customer
deposit customer amount = (getNI customer, getage customer, getbalance customer + amount)

-- Part c)
withdraw :: Customer -> Float -> Customer
withdraw customer amount
   | getbalance customer >= amount = (getNI customer, getage customer, getbalance customer - amount) 
   | otherwise = error "Customer does not have enough money in account"

-- Part d)
credit :: Bank -> [Customer]
credit bank = [ customer | customer <- bank, getbalance customer >=0]


-----------------------------------------------------------------
-- Exercise 3: 
-----------------------------------------------------------------

cubeOdds :: [Int]-> [Int]
cubeOdds xs = [ x*x*x | x <- xs, odd x]

cubeOdds2 :: [Int]-> [Int]
cubeOdds2 xs = map (\x -> x*x*x) onlyodd where onlyodd = filter odd xs

-----------------------------------------------------------------
-- Exercise 4: 
-----------------------------------------------------------------

repChar :: (Char, Char) -> String -> String
repChar pair text = map (\c -> if c == fst(pair) then snd(pair) else c) text

-----------------------------------------------------------------
-- Exercise 5: 
-----------------------------------------------------------------

-- Part a)
zap :: [Int] -> [Int] -> [(Int, Int)]
zap xs ys = zip xs ys

-- Part b)
addIndex :: [Int] -> [(Int, Int)]
addIndex xs = zip [1..length xs] xs

-- Part c)
blanks l n = take (l-n) (repeat ' ')
extend :: Int -> String -> String
extend k str
   | k > length str = str ++ blanks k (length str)
   | otherwise = str
