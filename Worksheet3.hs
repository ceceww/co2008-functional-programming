--------------------------------------------------------------------
-- CO2008  Functional Programming                            
-- Created: Feb 2017, University of Leicester, UK                        
--------------------------------------------------------------------           
-- Student Name: Cecelia Wisniewska
-- Student Number: 159021394
-- Student Login name: cw374
--------------------------------------------------------------------

--These question can and should be answered using all the functions as mentioned on the
--slides. Don't borrow library functions from the web. If in doubt ask the lecturers or TAs.
--After all the point is to teach you hwo to write this kind of code...

module Worksheet3 where 
import Data.Char
import Data.List

----------------------------------------------------------------------
-- Exercise 1
---------------------------------------------------------------------


skipall :: Int -> [a] -> [a] 
skipall n [] = []
skipall n xs
    |n>0 = take (n-1) xs ++ skipall n (drop n xs)
    |n<=0 = xs


----------------------------------------------------------------------
-- Exercise 2
---------------------------------------------------------------------

compareP :: String -> String -> Bool
compareP x "" = True
compareP (x:xs) (y:ys)
    |length(x:xs)>length(y:ys)&&(x==y) = compareP xs ys
    |otherwise = False

----------------------------------------------------------------------
-- Exercise 3
---------------------------------------------------------------------

--First write homerge that merges two sorted lists into a sorted list
--homerge :: Ord b => (a -> b) -> [a] -> [a] -> [a]
--homerge :: ([a] -> [a]) -> [a] -> [a] -> [a]
homerge sort [] ys = sort ys
homerge sort xs [] = sort xs
homerge sort xs ys = sort (sort xs ++ sort ys)

--Now write the higher order merge sort

--hoMergeSort :: Ord b => (a -> b)  -> [a] -> [a]

----------------------------------------------------------------------
-- Exercise 4
---------------------------------------------------------------------


type Lastname = String 
type Username = String 
type Mark = Int

type Spreadsheet = [(Lastname, Username, Mark)]


--sortLastname :: Spreadsheet ->  Spreadsheet

--sortUsername :: Spreadsheet ->  Spreadsheet

--sortMark :: Spreadsheet ->  Spreadsheet



----------------------------------------------------------------------
-- Exercise 5
---------------------------------------------------------------------


--smallest :: Ord a => a -> [a] -> a

--delete :: Ord a => a -> [a] -> [a]

--bucketsort :: Ord a => [a] ->  [a]


----------------------------------------------------------------------
-- Exercise 6
---------------------------------------------------------------------

--display :: [String] -> IO()


--triangle :: Int -> [String]

--test = display (triangle 3)



----------------------------------------------------------------------
-- Exercise 7
---------------------------------------------------------------------


--tree :: Int -> [String]

{-display(tree 3) should result in
   *
  ***
   *
  ***
 *****
   *
  ***
 *****
*******
-}



----------------------------------------------------------------------
-- Exercise 8
---------------------------------------------------------------------


--forest :: Int -> [String]


{- display (forest 3) should give as output exactly:
             *   
            ***  
             *   
            ***  
      *    ***** 
     ***     *   
      *     ***  
 *   ***   ***** 
*** ***** *******
-}