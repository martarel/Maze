Module to define the type of a maze

> module MyMaze (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography

We will represent a maze by its size and a list of its walls from each direction

The lists are for North, South, Weast and East

> data Maze = AMaze Size [Place] [Place] [Place] [Place]

> type MyWall = [Place]

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = AMaze (x,y) n s w e
>                      where allwalls = walls ++ map reflect walls
>                            n = [(i,y-1)| i <- [0..x-1]] ++ filter (/= (-1, -1)) [if d == N then (x,y) else if d == S then move d (i,j) else (-1, -1)| ((i, j), d) <- allwalls]
>                            s = [(i,0) | i <- [0..x-1]] ++ filter (/= (-1, -1)) [if d == S then (x,y) else if d == N then move d (i,j) else (-1, -1)| ((i, j), d) <- allwalls]
>                            w = [(0,j) | j <- [0..y-1]] ++ filter (/= (-1, -1)) [if d == W then (x,y) else if d == E then move d (i,j) else (-1, -1)| ((i, j), d) <- allwalls]
>                            e = [(x-1,j) | j <- [0..y-1]] ++ filter (/= (-1, -1)) [if d == E then (x,y) else if d == W then move d (i,j) else (-1, -1)| ((i, j), d) <- allwalls]


The following function "reflects" a wall; i.e. gives the representation as
seen from the other side; for example, reflect ((3,4), N) = ((3,5),S)

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

The following function tests whether the maze includes a wall in a particular
direction from a particular place:

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (AMaze _ n s w e) pos d | d == N = pos `elem` n
>                                | d == S = pos `elem` s
>                                | d == W = pos `elem` w
>                                | d == E = pos `elem` e

The following function returns the size of a maze:

> sizeOf :: Maze -> Size
> sizeOf (AMaze size _ _ _ _) = size


***************************************
*              Question 5            *
***************************************

*Results with MyMaze*

fastSolveMaze largeMaze (0, 0) (22, 21)
Right [N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.06 secs, 8,885,608 bytes)

solveMaze smallMaze (0,0) (3, 2)
[E,N,E,S,E,N,N]
(0.01 secs, 294,824 bytes)

fastSolveMaze impossibleMaze (0, 0) (2, 2)
Left "O(no)!"
(0.01 secs, 151,336 bytes)

*Results with Maze*

fastSolveMaze largeMaze (0, 0) (22, 21)
Right [N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.09 secs, 7,138,680 bytes)

solveMaze smallMaze (0,0) (3, 2)
[E,N,E,S,E,N,N]
(0.01 secs, 254,880 bytes)

fastSolveMaze impossibleMaze (0, 0) (3, 2)
Left "O(no)!"
(0.02 secs, 200,928 bytes)