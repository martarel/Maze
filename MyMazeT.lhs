Module to define the type of a maze

> module MyMazeT (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf, -- :: Maze -> Size
>   showTree,    
> )
> where


> import Geography
> import Data.List (sort)

We will represent a maze by its size and a binary tree of its walls from each direction

The trees are for North, South, West and East

> import qualified Data.List

> data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

contains :: (Tree Int) -> Int -> Bool

> contains Nil _ = False
> contains (Node t1 v t2) x 
>	| x == v = True
>	| x  < v = contains t1 x 
>	| x  > v = contains t2 x

insert :: (Tree Int) -> Int -> (Tree Int)

> insert Nil x = Node Nil x Nil
> insert (Node t1 v t2) x 
>	| v == x = Node t1 v t2
>	| v  < x = Node t1 v (insert t2 x)
>	| v  > x = Node (insert t1 x) v t2


> data Maze = TMaze Size (Tree Int) (Tree Int) (Tree Int) (Tree Int)

> type MyWall = [Place]


This was the non balanced tree

makeTree :: [Place] -> Int -> (Tree Int) -> (Tree Int)
makeTree [] size tree = tree
makeTree (x:xs) size tree = makeTree (xs) size (insert tree (convert x size))


to convert between a pair and an integer which can be inserted in the binary tree,
we will denote a pair (i, j) size as size*i + j
kinda like a number system in base size and the pair is its "binary" representation

> makeTree [] size = Nil
> makeTree elts size = Node (makeTree (take half elts) size) 
>                          (convert x size)
>                         (makeTree (drop (half+1) elts) size)
>    where half = length elts `quot` 2
>          x = (elts !! half) 


> convert (a, b) size = a*size + b

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze size@(x,y) walls = TMaze size treeN treeS treeW treeE
>                      where allwalls = walls ++ map reflect walls
>                            n = sort $ [(i,y-1)| i <- [0..x-1]] ++ filter (/= (-1, -1)) [if d == N then (x,y) else if d == S then move d (i,j) else (-1, -1)| ((i, j), d) <- allwalls]
>                            s = sort $ [(i,0) | i <- [0..x-1]] ++ filter (/= (-1, -1)) [if d == S then (x,y) else if d == N then move d (i,j) else (-1, -1)| ((i, j), d) <- allwalls]
>                            w = sort $ [(0,j) | j <- [0..y-1]] ++ filter (/= (-1, -1)) [if d == W then (x,y) else if d == E then move d (i,j) else (-1, -1)| ((i, j), d) <- allwalls]
>                            e =  sort $ [(x-1,j) | j <- [0..y-1]] ++ filter (/= (-1, -1)) [if d == E then (x,y) else if d == W then move d (i,j) else (-1, -1)| ((i, j), d) <- allwalls]
>                            treeN = makeTree n (maximum size)
>                            treeS = makeTree s (maximum size)
>                            treeW = makeTree w (maximum size) 
>                            treeE = makeTree e (maximum size) 


The following function "reflects" a wall; i.e. gives the representation as
seen from the other side; for example, reflect ((3,4), N) = ((3,5),S)

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

The following function tests whether the maze includes a wall in a particular
direction from a particular place:


The following function returns the size of a maze:

> sizeOf :: Maze -> Size
> sizeOf (TMaze size _ _ _ _) = size


> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (TMaze size n s w e) pos d | d == N = contains n $ convert pos (maximum(size))
>                                | d == S = contains s $ convert pos (maximum(size))
>                                | d == W = contains w $ convert pos (maximum(size))
>                                | d == E = contains e $ convert pos (maximum(size))


> showTree (TMaze size n s w e) = n


***************************************
*              Question 7            *
***************************************

fastSolveMaze largeMaze (0, 0) (22, 21)
Right [N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.08 secs, 46,294,048 bytes)

Not faster at all...


Okay I fixed the tree to be balanced now it's faster yay

fastSolveMaze largeMaze (0,0) (22, 21)
Right [N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.04 secs, 11,719,696 bytes)


