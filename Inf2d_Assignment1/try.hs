import TTTGame

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game
          | (terminal game == True) && (checkWin game 1 == True) = 1
          | (terminal game == True) && (checkWin game 0 == True) = -1
          | otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player
              | (terminal game == False) && maxPlayer player  = maximum[minimax p (switch player) | p <- moves game player]
              | (terminal game == False) && minPlayer player  = minimum[minimax p (switch player) | p <- moves game player]
              | otherwise = eval game
{-}
alphabeta:: Game->Player->Int
alphabeta game player
                | maxPlayer player = maxVal -2 2 v
                | otherwise = minimum [maxVal v a b | g <- moves game player]
                        where maxVal v a b
                                | _ = maxVal [] a' b'
                                | otherwise = maxVal v' a' b'
                                    where v' =
                                          a' =
                                          b' =
-}

loop::[Game] -> Int
--state = game player
alphabeta:: Game->Player->Int
alphabeta game player
                | maxPlayer player = maxVal game player (-2,2)
                | otherwise = minVal game player (-2,2)
                      where maxVal game player (a,b)
                              | terminal game = eval game
                              | otherwise =
                                  let (g:gs) = moves game player in

                                    loop (g:gs)
                                      | v < b = (max a v', v')
                                      | v' >= b = v'
                                      | otherwise = loop xs
                                        where v'= max (minVal g (switch player) (a'(max a v'),b)) v



                            minVal game player (a,b)
                              | terminal game = eval game
                              | otherwise =
                                  let (g:gs) = moves game player in
                                      loop (g:gs) (x,y)
                                        | v < b = (min a v', v')
                                        | v >= b = (a, v)
                                        | otherwise = loop xs
                                          where v'= min (maxVal g (switch player) (a,b)) v

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


                        alphabeta:: Game->Player->Int
                        alphabeta game player
                                  | maxPlayer player = maxVal game player (-2,2)
                                  | otherwise = minVal game player (-2,2)
                                        where maxVal game player (alpha,beta)

                                                | terminal game = eval game
                                                | otherwise = --output format
                                                  foldr (\g v -> if v >= beta then v else maximum[v, minVal game player (max a v, beta)] ) alpha (moves game player)
                                                          --initial alpha beta values (-2,-2)//iterate across a list of next states of maxPlayer

                                              minVal game player (alpha,beta)
                                                      | terminal game = eval game
                                                      | otherwise =
                                                        foldr (\g v -> if v <= a then v else minimum[v, maxVal game player (min a v, beta)] ) beta (moves game player)


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
