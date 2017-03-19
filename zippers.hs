data Tree a = ND | Data a | Branch (Tree a) (Tree a) deriving Show

data Dir = L | R deriving (Show)  
type Path = [Dir]  
type Breadcrumbs = [Dir]  

changeTo :: a -> Path -> Tree a -> Tree a
changeTo x (L:ds) (Branch l r) = Branch (changeTo x ds l) r  
changeTo x (R:ds) (Branch l r) = Branch l (changeTo x ds r)  
changeTo x [] (Data a) = (Data x)
changeTo _ _ _ = error "NOT ALLOWED U DIMWIT"

elemAt :: Path -> Tree a -> a  
elemAt (L:ds) (Branch l _) = elemAt ds l  
elemAt (R:ds) (Branch _ r) = elemAt ds r  
elemAt [] (Data a) = a
elemAt _ _ = error "No data"

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
goLeft (ND, bs) = (ND, bs)
goLeft (Branch l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (ND, bs) = (ND, bs)
goRight (Branch _ r, bs) = (r, R:bs)  

--Path acts as a sort of focus, because it pinpoints one exact sub-tree from our tree

tree1 = Branch 
            (Branch
                (Branch (Data 'C') ND) (Branch ND (Data 'E'))
            ) ND


