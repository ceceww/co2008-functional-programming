--The function boombang takes a list as input and replaces all odd numbers
-- <10 with "BOOM!" and all odd numbers >10 with "BANG!"
boombang xs = [ if x<10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--Multiply all elements in the list by 4
multiplyBy4 xs = [ x*4 | x <- xs ]

--Takes 2 lists of Strings as input and returns a list of elements of first list
--which are adjectives matched with elements of second list which are nouns
pair :: [String] -> [String] -> [String]
pair adjs nouns = [adj ++ " " ++ noun | adj <- adjs, noun <- nouns]

--Removes all upper case letters of the input String. Note that ' ' is included 
--in the predicate so that blank spaces are not removed.
removeUpperCase st = [ c | c <- st, c `elem` ['a'..'z']++[' ']]

--Replaces all elements of the list with 1 and sums that up to give list length
length' xs = sum [1 | _ <- xs]

--allChar :: Int -> IO()
--allChar n = putStr(map [1..n])

drops :: Int -> [Int] -> [Int]
drops n [] = []
drops 1 (x:xs) = xs
drops n xs
    | n>0 = drops 1 (drops (n-1) xs)
    | n<=0 = []

splitA :: [Int] -> [[Int]]
splitA [] = []
splitA xs = take 3 xs:splitA (drops 3 xs)

unsplitA :: [[Int]] -> [Int]
unsplitA [] = []
unsplitA (x:xs) = x++unsplitA xs

rmSnd:: [Int] -> [Int]
rmSnd (x:xs) = x:(tail xs)

takeWhileSmaller :: Int -> [Int] -> [Int]
takeWhileSmaller n [] = []
takeWhileSmaller n (x:xs)
    | x<n = x : takeWhileSmaller n xs
    | otherwise = takeWhileSmaller n xs

zop :: [a] -> [b] -> [(a,b)]
zop [] [] = []
zop [a] [] = []
zop [] [b] = []
zop (x:xs) (y:ys) = (x,y) : zop xs ys

unzop :: [(a,b)] -> ([a],[b])
unzop [] = ([],[])
unzop ((x,y):zs) = (x: fst(unzop zs),y: snd(unzop zs)) 

zUp :: [[Char]] -> [Int] -> [([Char], [Int])]
zUp (x:xs) (y:ys) = ('1':x,2:[y]):zUp xs ys

unzOp :: [(Int,Float)] -> ([Int],[Float])
unzOp ((x,y):zs) = ((x+1):(fst(unzOp zs)), (y+2.5): (snd(unzOp zs)))
