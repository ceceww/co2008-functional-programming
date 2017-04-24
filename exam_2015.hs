--Midsummer Examinations 2015

--Question 1

{-(a) What are the most general types of the following terms?

i. [(2,0),(1,5)]
[(Int)]

ii. (",",',',[']'])
([Char],Char,[Char])

iii. "20+15"
[Char]

iv. filter
(a -> Bool) -> [a] -> [a]

v. [tail ["a"]]
[[[Char]]]

(b) Give the (most general) type declarations for the following function definitions:
i. zip :: [a] -> [b] -> [(a,b)]
ii. zap :: [a] -> [b] -> [(b,a)]
iii. zop :: [a] -> [b] -> [(b,a)]

(c) Evaluate:
i. (head [1,2]) : []
[1]

ii. 1 + head(fst ([1,2],[3,2]))
2

iii. head [head [1..3] : tail [3..6]]
[1,4,5,6]

iv. filter (==3) (tail [3, fst(1,2), 1+2, snd(1,2)])
3

-}

--(d)
--i. What is the most general type of filter?
--filter :: (a -> Bool) -> [a] -> [b]

--ii. Write down the function of filter using recursion.
filter' f [] = []
filter' f (a:as)
   | f a == True = a: (filter' f as)
   | otherwise = filter' f as

--iii. Write down the function of filter using list comprehension.
filter'' f [] = []
filter'' f list = [ x | x <- list, f x == True ]

--(e)

--i.
flatten :: [[a]] -> [a]
flatten [] = []
flatten (a:as) = a ++ flatten as

--ii.
elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs

--Question 2

type RawText = String
type Word' = String
type Line = [Word']
type Page = [Line]
  
dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
  | x == ' ' = xs
  | otherwise = dropWord xs
  
dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
  | x == ' ' = dropSpace xs
  | otherwise = (x:xs)

getWord :: String -> String
getWord [] = []
getWord (x:xs)
  | x == ' ' = []
  | otherwise = x : (getWord xs)
  
split :: RawText -> [Word']
split [] = []
split list
  | getWord list == "" = split (tail list)
  | otherwise = getWord list : split (dropSpace (dropWord list))
--split " one two three  ff " = ["one","two","three","ff"]

getL :: Int -> [Word'] -> Line
getL len [] = []
getL len (x:xs)
 | length x <= len = x : restOfLine
 | otherwise = []
   where 
   newlen = len - (length x + 1)
   restOfLine = getL newlen xs
   
wordz = split ("We want to write Haskell code that can typeset a given text string as a (single) page of lines of at most 30 characters.")
   
wrap :: [Word'] -> Page
wrap [] = []
wrap words = getL 30 words : wrap (drop (length (getL 30 words)) words)

--page2string [["one","two","three","four"],["five","six"]] = "one two three four\nfive six".

unsplit :: [Word'] -> RawText
unsplit [] = []
unsplit (x:xs) = x ++ " " ++ unsplit xs

page = [["one","two","three","four"],["five","six"]]

page2string :: Page -> String
page2string [] = []
page2string (x:xs) = unsplit x ++ "\n" ++ page2string xs

typeset :: String -> IO()
typeset str = putStr (page2string (wrap (split str)))

data Tree a = L a | N a (Tree a) (Tree a) deriving Show
data Dir = F | M deriving Show
type Path = [Dir]

ftree = N "Anna" (N "Fer-Jan" (L "Willem") (L "Nettie")) (L "Paula")

addPath :: Tree a -> Tree (Path,a)
addPath (L a) = L ([], a)
addPath (N x (L a) rt) = ((N ([], x)) (L ([F], a)) (addPath rt))
addPath (N x lt (L a)) = ((N ([], x)) (addPath lt) (L ([M], a)))

traverseDF :: Tree a -> [a]
traverseDF (L a)       = a : []
traverseDF (N a l r) = a : (traverseDF l) ++ (traverseDF r)
