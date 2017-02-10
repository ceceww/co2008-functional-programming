--------------------------------------------------------------------
-- CO 2008  Functional Programming  
-- Created: January 2016, University of Leicester, UK                        
-------------------------------------------------------------------- 
--
-- DON'T FORGET TO FILL IN NAME AND STUDENT NUMBER.
--                       
--------------------------------------------------------------------           
-- Student Name: Cecelia Wisniewska
-- Student Number: 159021394
--------------------------------------------------------------------

module Worksheet0 where 
import Data.Char
--------------------------------------------------------------------
-- Exercise 6
--------------------------------------------------------------------


myint :: Int
myint = 707


myfloat :: Float
myfloat = 12.5


mychar :: Char 
mychar = 't'


mystring :: String
mystring = "Hello "

less :: Bool
less = (myint < 100)


cube :: Int -> Int
cube n = n*n*n

--------------------------------------------------------------------
-- Exercise 7
--------------------------------------------------------------------


-- A function with two integer input that adds them.
plus :: Int -> Int -> Int 
plus m n = m+n


-- A function with three integer inputs and Boolean output;
-- yields True if all inputs equal, else False.
allEqual :: Int -> Int -> Int -> Bool
allEqual m n k =  if m==n && n==k && m==k then True else False



--------------------------------------------------------------------
-- Exercise 10
--------------------------------------------------------------------


message :: Float -> String
message x = "The number is " ++ show(x)


--------------------------------------------------------------------
-- Exercise 11
--------------------------------------------------------------------

--Write a function blankVowel that given a character x returns a blank
--when x is a vowel (ie. x ∈ {a, e, i, o, u, A, . . .}) and other wise returns x.

blankVowel :: Char -> Char
blankVowel x = if x `elem` ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'] then ' ' else x

text :: String
text = "Altijd is Kortjakje ziek,\ndriemaal in de week,\nmaar zondags niet.\n"

-------------------------------------------------------------------
-- Exercise 12 Challenge
--------------------------------------------------------------------

--Write a function blankVowel2 that given a character x returns a blank
--when x is a DUTCH vowel (ie. x ∈ {a, e, i, ij, o, u, A, . . .}) and other wise returns x.
-- in the Dutch language also the combination ij is treated as a vowel!

--charToString :: Char -> String
--charToString c = [c]

blankVowel2 :: String -> String
blankVowel2 x
     |x `elem` ["a", "e", "i", "o", "u", "A", "E", "I", "O", "U"] = [' ']
     |x `elem` ["ij"] = "  "
     |otherwise = x

