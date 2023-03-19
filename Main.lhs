
> import Geography
> import MyMazeT
> import Maybe
> import Data.List (nubBy)

======================================================================

Draw a maze.

***************************************
*              Question 2             *
* Complete the definition of drawMaze *
***************************************


> getGrid maze = drawRight (2*row+1) (drawLeft (2*row+1) (drawBottom (col*3-1) (drawTop (col*3-1) (replicate (row*2-1) . replicate (col*3-1) $ " "))))
>                where (col, row) = sizeOf maze

> drawMaze :: Maze -> IO()
> drawMaze maze = putStr $ unlines $ map unwords grid 
>                 where (col, row) = sizeOf maze
>                       grid = drawNorthSouth maze $ drawWestEast maze $ drawRight (2*row+1) $ drawLeft (2*row+1) $ drawBottom (col*3-1) $ drawTop (col*3-1) (replicate (row*2-1) . replicate (col*3-1) $ " ")

so the actual dimensions of the grid is (2*row+1) (3*col+1)
a square is actually two columns wide and one row high

> cols :: [[a]] -> [[a]]
> cols = foldr (zipWith (:)) (repeat [])

> drawTop col grid = (take col (cycle ["-", "-", "+"])) : grid 
> drawBottom col grid = reverse ((take col (cycle ["-", "-", "+"])) : (reverse grid))
> drawLeft row grid = cols ((take row (cycle ["+", "|"])) : (cols grid))
> drawRight row grid = cols (reverse((take row (cycle ["+", "|"])) : (reverse (cols grid))))


> updateGrid:: [[a]] -> a -> (Int, Int) -> [[a]]
> updateGrid grid x (r,c) =
>  take r grid ++
>  [take c (grid !! r) ++ [x] ++ drop (c + 1) (grid !! r)] ++
>  drop (r + 1) grid


> addWestEast' i j maze row col grid | hasWall maze (j, i) E = updateGrid (updateGrid (updateGrid grid "|" (2*row-1-i*2, (j*3+3))) "+" (2*row-i*2, (j*3+3))) "+" (2*row-i*2-2, (j*3+3))
>                                    | otherwise = grid

> drawWestEast' i j maze row col grid | i >= row = grid
>                                     | j >= col = drawWestEast' (i+1) 0 maze row col newGrid
>                                     | otherwise = drawWestEast' i (j+1) maze row col newGrid
>                                    where newGrid = addWestEast' i j maze row col grid

                          
> drawWestEast maze grid = drawWestEast' 0 0 maze row col grid
>                       where (col, row) = sizeOf maze


> addNorthSouth' i j maze row col grid | hasWall maze (j, i) S = updateGrid (updateGrid (updateGrid (updateGrid grid "-" ((2*row-2*i), (j*3+1)))  "-" ((2*row-2*i), (j*3+2)))  "+" ((2*row-2*i), (j*3+3)))  "+" ((2*row-2*i), (j*3))
>                                    | otherwise = grid

> drawNorthSouth' i j maze row col grid | i >= row && j >= col = grid
>                                     | j >= col = drawNorthSouth' (i+1) 0 maze row col newGrid
>                                     | otherwise = drawNorthSouth' i (j+1) maze row col newGrid
>                                    where newGrid = addNorthSouth' i j maze row col grid

                          
> drawNorthSouth maze grid = drawNorthSouth' 0 0 maze row col grid
>                       where (col, row) = sizeOf maze



======================================================================

Solve the maze, giving a result of type:

> type Path = [Direction]

***************************************
*            Questions 3--4           *
*     Complete the definition of      *
*              solveMaze              *
***************************************


> found :: Place -> [(Place, Path)] -> (Bool, Path)
> found target list = if lookup target list == Nothing then (False, []) else (True, fromJust (lookup target list))

> solveMazeIter :: Maze -> Place -> [(Place, Path)] -> Path
> solveMazeIter maze target list = if fst $ found target nextList then reverse $ snd $ found target nextList else solveMazeIter maze target nextList
>                              where nextList = concat [next maze (place, path) | (place, path) <- list]

> next :: Maze -> (Place, Path) -> [(Place, Path)]
> next maze (place, path) = north ++ south ++ west ++ east ++ []
>                       where north = next' maze (place, path) N 
>                             south = next' maze (place, path) S
>                             east = next' maze (place, path) E
>                             west = next' maze (place, path) W


> next' maze (place, path) direction = if hasWall maze place direction then [] else [(move direction place, direction:path)]


> solveMaze :: Maze -> Place -> Place -> Path
> solveMaze maze start target = solveMazeIter maze target [(start, [])]


It works!

solveMaze smallMaze (0, 0) (3, 2)
[E,N,E,S,E,N,N]


======================================================================

this works too!!!

> fastSolveMazeIter :: Maze -> Place -> [(Place, Path)] -> [Place] -> Either String Path
> fastSolveMazeIter maze target list visited = if fst $ found target nextList then Right (reverse $ snd $ found target nextList) else if (length visited == length nextVisited) then Left "O(no)!" else fastSolveMazeIter maze target nextList nextVisited
>                              where nextList = nubBy (\(x,_) (x', _) -> x == x') $ concat [next'' maze (place, path) visited | (place, path) <- list]
>                                    nextVisited = visited ++ [fst elem | elem <- nextList]


> fastSolveMaze :: Maze -> Place -> Place -> Either String Path
> fastSolveMaze maze start target = fastSolveMazeIter maze target [(start, [])] [start]

> next'' :: Maze -> (Place, Path) -> [Place] -> [(Place, Path)]
> next'' maze (place, path) visited = north ++ south ++ west ++ east ++ []
>                       where north = next''' maze (place, path) N visited
>                             south = next''' maze (place, path) S visited
>                             east = next''' maze (place, path) E visited
>                             west = next''' maze (place, path) W visited

> next''' maze (place, path) direction visited = if hasWall maze place direction || (move direction place) `elem` visited then [] else [(move direction place, direction:path)]


======================================================================

Some test mazes.  In both cases, the task is to find a path from the bottom
left corner to the top right.

First a small one

> smallMaze :: Maze
> smallMaze = 
>   let walls = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E), 
>                ((1,2), E), ((1,1), N)]
>   in makeMaze (4,3) walls

> emptyMaze :: Maze
> emptyMaze = 
>   let walls = []
>   in makeMaze (5, 5) walls

Now a large one.  Define a function to produce a run of walls:

> run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
> run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]

And here is the maze.

> largeMaze :: Maze 
> largeMaze =
>   let walls = 
>         run (0,0) 3 E ++ run (1,1) 3 E ++ [((1,3),N)] ++ run (0,4) 5 E ++
>         run (2,0) 5 E ++ [((2,4),N)] ++ run (1,5) 3 E ++
>         run (1,8) 3 N ++ run (2,6) 3 E ++
>         run (3,1) 7 E ++ run (4,0) 4 N ++ run (4,1) 5 E ++ run (5,2) 3 N ++
>         run (4,6) 2 N ++ run (5,4) 3 E ++ run (6,3) 5 N ++ run (8,0) 4 E ++
>         run (6,1) 3 N ++ run (0,9) 3 N ++ run (1,10) 3 N ++ run (0,11) 3 N ++
>         run (1,12) 6 N ++ run (3,9) 4 E ++ run (4,11) 2 N ++
>         run (5,9) 3 E ++ run (4,8) 3 E ++ run (5,7) 5 N ++ run (6,4) 9 E ++
>         run (7,5) 3 N ++ run (8,4) 4 N ++ run (8,6) 3 N ++ run (10,5) 7 E ++
>         run (9,8) 3 E ++ run (8,9) 3 E ++ run (7,8) 3 E ++ run (8,11) 3 N ++
>         run (0,13) 5 N ++ run (4,14) 2 E ++ run (0,15) 2 E ++ 
>         run (1,14) 3 N ++ run (3,15) 2 E ++ run (0,17) 2 N ++ 
>         run (1,16) 2 E ++ run (2,15) 1 N ++ run (3,16) 3 N ++
>         run (2,17) 2 E ++ run (1,18) 6 N ++ run (4,17) 3 N ++ 
>         run (6,14) 7 E ++ run (5,13) 4 E ++ run (7,12) 2 E ++
>         run (8,13) 3 N ++ run (7,14) 3 N ++ run (10,14) 2 E ++
>         run (8,15) 5 N ++ run (7,16) 5 N ++ run (9,1) 2 E ++
>         run (10,0) 12 N ++ run (21,1) 1 E ++ run (10,2) 2 E ++
>         run (11,1) 7 N ++ run (17,1) 1 E ++ run (11,3) 3 E ++
>         run (12,2) 7 N ++ run (18,2) 2 E ++ run (19,1) 2 N ++
>         run (15,3) 3 N ++ run (14,4) 3 E ++ run (13,3) 3 E ++
>         run (12,4) 3 E ++ run (12,6) 3 N ++ run (11,7) 8 E ++ 
>         run (9,12) 3 N ++ run (12,14) 1 N ++ run (12,8) 10 E ++
>         run (0,19) 6 N ++ run (1,20) 6 N ++ run (7,18) 8 E ++
>         run (8,17) 1 N ++ run (8,18) 3 E ++ run (9,17) 4 E ++ 
>         run (10,18) 2 E ++ run (11,17) 2 E ++ run (10,20) 3 N ++
>         run (11,19) 3 N ++ run (12,18) 2 N ++ run (13,17) 2 N ++
>         run (13,13) 4 E ++ run (14,12) 7 N ++ run (13,11) 2 N ++
>         run (14,10) 2 E ++ run (13,9)2 E ++ run (14,8) 3 N ++ 
>         run (13,7) 3 N ++ run (15,5) 3 E ++ run (16,6) 3 E ++
>         run (18,5) 4 N ++ run (16,4) 2 N ++ run (13,20) 2 E ++
>         run (14,18) 4 E ++ run (20,2) 3 N ++ run (19,3) 2 E ++
>         run (18,4) 2 E ++ run (23,4) 1 E ++ run (22,4) 1 N ++
>         run (21,3) 1 N ++ run (20,4) 2 E ++ run (17,6) 4 N ++ 
>         run (20,7) 2 E ++ run (21,7) 2 N ++ run (21,6) 1 E ++ 
>         run (15,9) 1 E ++ run (17,8) 2 E ++ run (18,7) 2 E ++ 
>         run (19,8) 2 E ++ run (21,9) 1 E ++ run (16,9) 6 N ++
>         run (16,10) 7 N ++ run (15,11) 2 E ++ run (17,11) 5 N ++ 
>         run (14,14) 3 E ++ run (15,15) 6 E ++ run (17,14) 4 E ++
>         run (16,18) 4 E ++ run (15,17) 1 N ++ run (17,17) 3 N ++
>         run (15,13) 7 N ++ run (21,12) 2 E ++ run (16,16) 1 N ++
>         run (16,14) 1 N ++ run (17,15) 3 N ++ run (19,14) 4 N ++
>         run (20,15) 5 E ++ run (19,16) 2 N ++ run (21,16) 5 E ++
>         run (17,19) 2 E ++ run (18,20) 2 E ++ run (19,19) 2 E ++
>         run (18,18) 2 N ++ run (20,20) 3 N
>   in makeMaze (23,22) walls

And now an impossible maze

> impossibleMaze :: Maze
> impossibleMaze =
>   let walls = [((0,1), E), ((1,0),N), ((1,2), E), ((2,1), N)]
>   in makeMaze (3,3) walls


***************************************
*            Questions 6           *        
***************************************


Generated using generate.py because random numbers in haskell are just ugh

> randomMaze :: Maze
> randomMaze = 
>   let walls = [((1, 2), N), ((4, 8), E), ((6, 2), E), ((6, 4), S), ((6, 5), W), ((8, 4), S), ((8, 5), W)]
>  in makeMaze (10, 10) walls


fastSolveMaze randomMaze (0,0) (9, 9)
Right [N,N,N,N,N,N,N,N,N,E,E,E,E,E,E,E,E,E]
(0.03 secs, 1,277,608 bytes)


