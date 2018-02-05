:- [war_of_life]. 
:- use_module(library(system)).

test_strategy(N, Strat1, Strat2) :-
   now(StartTime),
   test_strategy(N, Strat1, Strat2, Moves, Results),
   now(EndTime),
   Time is (EndTime-StartTime)*1000/N,
   count_elem(Results, 'draw', NofDraws),
   count_elem(Results, 'b', NofBlueWins),
   count_elem(Results, 'r', NofRedWins),
   format('Draws:     ~w~n',[NofDraws]),
   format('Blue wins: ~w~n',[NofBlueWins]),
   format('Red wins:  ~w~n',[NofRedWins]),
	format('Average game time: ~w ms ~n',[Time]).

test_strategy(0, _, _, [], []).

test_strategy(N, S1, S2, [NofMoves |Moves], [WinningPlayer |Results]) :-
  
   play(quiet, S1, S2, NofMoves, WinningPlayer),
   NewN is N-1,
   
   test_strategy(NewN, S1, S2,  Moves,  Results).

count_elem([], H, 0).
count_elem([H|T], H, Num) :- 
   count(T,H,Z), 
   Num is Z+1.
count_elem([Hbis|T], H, Num) :- 
   Hbis \= H,
   count(T,H,Num).

shortest_game([H|Moves], Result) :-  
   shortest_game(Moves, H, Result).

shortest_game([], Result, Result).

shortest_game([H|Moves], Shortest, Result) :- 
   (H < Shortest ->
      shortest_game(Moves, H, Result);
      shortest_game(Moves, Shortest, Result)).
