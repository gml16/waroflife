:- [war_of_life]. 
:- use_module(library(system)).

test_strategy(N, Strat1, Strat2) :-
   
   test_strategy(N, Strat1, Strat2, Moves, Results, 0, Time),
   
   count_elem(Results, 'draw', NofDraws),
   count_elem(Results, 'b', NofBlueWins),
   count_elem(Results, 'r', NofRedWins),
   shortest_game(Moves, Shortest),
   longest_game(Moves, Longest),
   average_game_length(Moves, Average),
   AverageTime is Time / N,
   format('Draws:     ~w~n',[NofDraws]),
   format('Blue wins: ~w~n',[NofBlueWins]),
   format('Red wins:  ~w~n',[NofRedWins]),
   format('Longest game:  ~w~n',[Longest]),
   format('Shortest game:  ~w~n',[Shortest]),
   format('Average game length:  ~w~n',[Average]),
	format('Average game time: ~w ms ~n',[AverageTime]).
   

test_strategy(0, _, _, [], [], Time, Time).

test_strategy(N, S1, S2, [NofMoves |Moves], [WinningPlayer |Results], AccTime, TotalTime) :-
   statistics(runtime,[StartTime|_]),
   play(quiet, S1, S2, NofMoves, WinningPlayer),
   statistics(runtime,[EndTime|_]),
   Time is (EndTime - StartTime),
   NewAcc is (AccTime + Time),
   NewN is N-1,
   test_strategy(NewN, S1, S2,  Moves,  Results, NewAcc, TotalTime).

count_elem([], _, 0).
count_elem([H|T], H, Num) :- 
   count_elem(T,H,Z), 
   Num is Z+1.
count_elem([Hbis|T], H, Num) :- 
   Hbis \= H,
   count_elem(T,H,Num).

average_game_length(Moves, Result) :-
   sumlist(Moves, Sum),
   length(Moves, Length),
   Length > 0, 
   Result is Sum / Length.

shortest_game([H|Moves], Result) :-  
   shortest_game(Moves, H, Result).

shortest_game([], Result, Result).

shortest_game([H|Moves], Shortest, Result) :- 
   (H < Shortest ->
      shortest_game(Moves, H, Result);
      shortest_game(Moves, Shortest, Result)).

longest_game([H|Moves], Result) :-  
   longest_game(Moves, H, Result).

longest_game([], Result, Result).

longest_game([H|Moves], Longest, Result) :- 
   ((H > Longest, H < 250) ->
      longest_game(Moves, H, Result);
      longest_game(Moves, Longest, Result)).





bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
   calculate_next_play(PlayerColour, CurrentBoardState, bloodlust, NewBoardState, Move).

calculate_next_play(PlayerColour, CurrentBoardState, Strategy, NewBoardState, Move) :-
   separate_colours(PlayerColour, CurrentBoardState, CurrentPlayer, Opponent),
   list_all_moves(CurrentPlayer, Opponent, AllMoves),
   find_best_move(AllMoves, Strategy, PlayerColour, CurrentBoardState, Move),
   move_a_piece(Move, PlayerColour, CurrentBoardState, NewBoardState).

list_all_moves(Alive, OtherPlayerAlive, AllMoves) :-
   findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
	              \+member([MA,MB],Alive),
	              \+member([MA,MB],OtherPlayerAlive)),
	 AllMoves).


separate_colours('b', [B, R], B, R).
separate_colours('r', [B, R], R, B).

regroup_colours('b', Player, Opponent, [Player | [Opponent]]).
regroup_colours('r', Player, Opponent, [Opponent | [Player]]).


/* Calculate the fitness of head element and then calculate once the fitness of every other move, keeping track of the best move so far as well as its fitness */
find_best_move([H | OtherMoves], Strategy, PlayerColour, CurrentBoardState, Move) :-
   move_a_piece(H, PlayerColour, CurrentBoardState, BoardAfterMove),
   next_generation(BoardAfterMove, BoardAfterCrank),
   fitness_score(Strategy, PlayerColour, BoardAfterCrank, Fitness),
   find_best_moves(OtherMoves, Strategy, PlayerColour, CurrentBoardState, H, Fitness, Move).

find_best_moves([], _, _, _, BestMove, _, BestMove).

find_best_moves([H | OtherMoves], Strategy, PlayerColour, CurrentBoardState, SoFarMove, SoFarFitness, Move) :-
   move_a_piece(H, PlayerColour, CurrentBoardState, BoardAfterMove),
   next_generation(BoardAfterMove, BoardAfterCrank),
   fitness_score(Strategy, PlayerColour, BoardAfterCrank, Fitness),
   (Fitness > SoFarFitness ->
      find_best_moves(OtherMoves, Strategy, PlayerColour, CurrentBoardState, H, Fitness, Move);
      find_best_moves(OtherMoves, Strategy, PlayerColour, CurrentBoardState, SoFarMove, SoFarFitness, Move)).

fitness_score(bloodlust, PlayerColour, Board, Fitness) :-
   separate_colours(PlayerColour, Board, _, Opponent),
   length(Opponent, Length),
   Fitness is -Length.


move_a_piece(Move, PlayerColour, CurrentBoardState, NewBoardState) :-
   separate_colours(PlayerColour, CurrentBoardState, CurrentPlayer, Opponent),
   alter_board(Move, CurrentPlayer, NewCurrentPlayer),
   regroup_colours(PlayerColour, NewCurrentPlayer, Opponent, NewBoardState).
   










