-- Inf2d Assignment 1 2018-2019
-- Matriculation number: s1770036
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame


gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = []

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth= 35
-- Why did you choose this number?
-- Because the grid is 6x6 hence a 36 tiled grid, and the starting node takes up 1 tile, hence 35 possible steps to take left.


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] = []
next branch =
  [nl: branch | nl <- nls]
      where
        nls = filter noDup(filter badNodesCheck(filter checkRange [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]))
              where
                checkRange n = (fst(n) `elem` [1..6] && snd(n) `elem` [1..6])     --make sure in grid
                badNodesCheck n = notElem n badNodesList  --removes badNodes
                noDup n = notElem n branch                --removes dupilcates, compare with input branch
                x = fst(head(branch))
                y = snd(head(branch))

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration filter ((>0).fst.head)  for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.
breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch _ next [] _ = Nothing
breadthFirstSearch destination next (b:bs) exploredList
                      | checkArrival destination (head b)  = Just b
                      -- if currNode is not explored yet, 1.prepend expanded branch to tree, 2.update exploredList
                      | head b `notElem` exploredList = breadthFirstSearch destination next (bs ++ (next b)) ((head b):exploredList)
                       --expand other branches except first whose currNode has already been explored
                      | otherwise = breadthFirstSearch destination next bs ((head b):exploredList)


-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch _ next [] _ = Nothing
depthFirstSearch destination next (b:bs) exploredList
                      | checkArrival destination (head b)  = Just b
                      -- if currNode is not explored, 1.append its expanded branch, this branch holds newest nodes to be expanded, 2.update explored List
                      | head b `notElem` exploredList = depthFirstSearch destination next ((next b) ++ bs) ((head b):exploredList)
                      --if currNode has alr been explored, expand its next branches
                      | otherwise = depthFirstSearch destination next bs ((head b):exploredList)

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch _ next [] _ = Nothing
depthLimitedSearch destination next (b:bs) d
                      | checkArrival destination (head b) = Just b
                      | d == 0 && not(checkArrival destination (head b)) = depthLimitedSearch destination next (bs) d --if leaf of tree is reached, expand remaining branches
                      | otherwise = depthLimitedSearch destination next ((next b) ++ bs) (d-1)



-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
                    --initial Node == goal?
                    | checkArrival destination initialNode = Just [initialNode]
                    --if DLS with this 'd' and can't find solution, then iDS w/ updated 'd'
                    | depthLimitedSearch destination next [[initialNode]] d == Nothing = iterDeepSearch destination next initialNode (d+1)
                     --if DLS with this 'd' and can't find solution, and depth limit hits the predefined maxDepth, return "no solution is found"
                    | d == maxDepth && (depthLimitedSearch destination next [[initialNode]] d) == Nothing = Nothing
                    --when DLS found a solution, output it
                    | otherwise = depthLimitedSearch destination next [[initialNode]] d
-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan (xp,yp) (xd,yd) = abs(xp - xd) + abs(yp - yd)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.



bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch _ next heuristic [] _ = Nothing
bestFirstSearch destination next heuristic (b:bs) exploredList
                    | checkArrival destination (head b) = Just b
                    --expand b, if it has not been explored, sort this [Branch] to ascending Branches based on its heuristics.
                    | head b `notElem` exploredList = bestFirstSearch destination next heuristic (sortedFunc(next b) ++ bs) ((head b):exploredList)
                    | otherwise = bestFirstSearch destination next heuristic bs ((head b):exploredList)
                            where
                              --sortedFunc : calculates each branch's heuristic value, and sort the branches in ascending order
                              sortedFunc::[Branch]->[Branch]
                              sortedFunc branches = [ y |(x,y) <-(sortBy (\(a,_) (b,_) -> compare a b) zippedList)] -- after sorted, extract only the branches, which is the second item of each pair
                                  where
                                    zippedList = zip (map (\p -> heuristic(head p)) branches) branches -- type[(a0, Branch)]; each branch paired with its corresponding heuristic value

--  filter out branch with the smallest currNode h(n) [list of branches]

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost (b:bs) exploredList
                    | checkArrival destination (head b) = Just b
                    | head b `notElem` exploredList = aStarSearch destination next heuristic cost (sortedFunc(next b) ++ bs) ((head b):exploredList)
                    | otherwise = aStarSearch destination next heuristic cost bs ((head b):exploredList)
                            where
                              sortedFunc::[Branch]->[Branch]
                              sortedFunc branches = [ y |(x,y) <-(sortBy (\(a,_) (b,_) -> compare a b) zippedList)]
                                  where
                                    zippedList = zip (map (\p -> heuristic(head p) + cost(p)) branches) branches --extra: include the cost


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length(branch)


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game
          | (terminal game == True) && (checkWin game 1 == True) = 1 --when player 1 wins
          | (terminal game == True) && (checkWin game 0 == True) = -1 --player 0 wins
          | otherwise = 0 --draw

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player
              | (terminal game == False) && maxPlayer player  = maximum[minimax p (switch player) | p <- moves game player] --choose the maximum of the minimums(of minPlayer)
              | (terminal game == False) && minPlayer player  = minimum[minimax p (switch player) | p <- moves game player] --choose the minimum of the maximums(of maxPlayer)
              | otherwise = eval game

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Game->Player->Int
alphabeta game player
    | maxPlayer player = maxVal game player (-2,2)
    | otherwise = minVal game player (-2,2)
    where maxVal game player (alpha,beta)

            | terminal game = eval game
            | otherwise = --output format (alpha,beta)
              snd ( foldr (\g (a,v) -> (
                  let
                      v' = max (minVal g (switch player) (a,beta)) v
                  in
                      if v >= beta
                      then (a,v)
                      else (max a v',v')

              )) (alpha,-2) (moves game player)) --initial alpha beta values (-2,-2)//iterate across a list of next states of maxPlayer

          minVal game player (alpha,beta)
                  | terminal game = eval game
                  | otherwise =

                    snd ( foldr (\g (b,v) -> (
                        let
                            v' = min (maxVal g (switch player) (alpha,b)) v -- newV value is the minimum of oldV vs. terminal utility value
                        in
                            if v <= alpha
                            then (b,v) -- ignore other nodes if evaluated game state has lower util value than alpha so far
                            else (min b v',v')

                    )) (beta,2) (moves game player)) --initial alpha beta values (2,2)//iterate across a list of next states of minPlayer


-- | Section 5.2 Wild Tic Tac Toe

-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game
            | (terminal game == True) && (checkWin game 1 == True || checkWin game 0 == True) = 1 --when player 1 or 0 wins
            | otherwise = 0 --draw

-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

alphabetaWild:: Game->Player->Int
alphabetaWild game player
      | maxPlayer player = maxVal game player (-2,2)
      | otherwise = minVal game player (-2,2)
            where maxVal game player (alpha,beta)
                          | terminal game = send game player
                          | otherwise =
                              snd ( foldr (\g (a,v) -> (
                                  let
                                      v' = max (minVal g (switch player) (a,beta)) v
                                  in
                                      if v >= beta
                                      then (a,v)
                                      else (max a v',v')

                              )) (alpha,-2) (moves game player)) --initial alpha beta values (-2,-2)//iterate across a list of next states of maxPlayer
                              where send ga pl
                                        | evalWild ga == 1 && maxPlayer pl = 1
                                        | evalWild ga == 1 && minPlayer pl = -1
                                        | otherwise = 0
                  minVal game player (alpha,beta)
                          | terminal game = send game player
                          | otherwise =

                              snd ( foldr (\g (b,v) -> (
                                  let
                                      v' = min (maxVal g (switch player) (alpha,b)) v -- newV value is the minimum of oldV vs. terminal utility value
                                  in
                                      if v <= alpha
                                      then (b,v) -- ignore other nodes if evaluated game state has lower util value than alpha so far
                                      else (min b v',v')

                              )) (beta,2) (moves game player)) --initial alpha beta values (2,2)//iterate across a list of next states of minPlayer
                              where send ga pl
                                        | evalWild ga == 1 && maxPlayer pl = 1
                                        | evalWild ga == 1 && minPlayer pl = -1
                                        | otherwise = 0



-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined

			-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
