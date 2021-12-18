:- consult('renpaarden.pl').

% função de teste

board(0, [[ 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
          [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 2, 2, 2, 2, 2, 2, 2, 2, 2 ],
          [ 2, 2, 2, 2, 2, 2, 2, 2, 2 ]]).

board(1, [[ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 2, 2, 2, 2, 2, 2, 2, 2, 2 ],
          [ 2, 2, 2, 2, 2, 2, 2, 2, 2 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
          [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ]]).

board(2, [[ 2, 2, 2, 2, 2, 2, 2, 2, 2 ],
          [ 2, 2, 2, 2, 2, 2, 2, 2, 2 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
          [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]]).

board(3, [[ 2, 2, 2, 2, 2, 2, 2, 2, 2 ],
          [ 2, 2, 2, 2, 2, 2, 2, 2, 2 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 1, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 1, 1, 1, 1, 1, 1, 1, 1 ],
          [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ]]).

board(4, [[ 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
          [ 1, 1, 0, 1, 1, 1, 1, 1, 1 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 1, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 0, 2, 0, 0, 0, 0 ],
          [ 2, 2, 2, 2, 0, 2, 2, 2, 2 ],
          [ 2, 2, 2, 2, 2, 2, 2, 2, 2 ]]).

board(5, [[ 1, 1, 1, 1, 1 ],
          [ 1, 1, 0, 1, 1 ],
          [ 0, 0, 0, 0, 0 ],
          [ 0, 0, 1, 1, 0 ],
          [ 0, 0, 0, 0, 0 ],
          [ 0, 0, 0, 2, 0 ],
          [ 2, 2, 2, 2, 2 ]]).

test_no_winner(Winner) :- board(0, Board), game_over(Board-1, Winner).
test_winner_1(Winner) :- board(1, Board), game_over(Board-1, Winner).
test_winner_2(Winner) :- board(2, Board), game_over(Board-1, Winner).

test_unvalid_move :- board(4, Board), valid_move(1, 4-3, 4-1, Board).
test_valid_move :- board(4, Board), valid_move(1, 4-3, 5-6, Board).
test_all_valid_moves(ValidMoves) :- board(4, Board), valid_move(1, 4-3, ValidMoves, Board).

test_correct_move1(NewGameState) :- board(4, Board), move(Board-1, 4-3-5-6, NewGameState).
test_correct_move2(NewGameState) :- board(4, Board), move(Board-2, 7-1-5-0, NewGameState).
test_correct_move3(NewGameState) :- board(5, Board), move(Board-1, 3-2-4-1, NewGameState).
test_uncorrect_move1(NewGameState) :- board(4, Board), move(Board-1, 4-3-4-1, NewGameState).
test_uncorrect_move2(NewGameState) :- board(4, Board), move(Board-1, 4-3-6-4, NewGameState).
test_uncorrect_move3(NewGameState) :- board(4, Board), move(Board-2, 4-3-5-6, NewGameState).

viewTab([]).
viewTab([H|T]) :-
    printList(H),
    viewTab(T).

printList([]) :-
    nl.
printList([H|T]) :-
    write(H),
    write(' | '),
    printList(T).
    