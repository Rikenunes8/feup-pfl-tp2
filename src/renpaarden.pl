:-use_module(library(lists)).


/*
definição da representação interna do estado do jogo, 
implementação dos predicados de validação e execução de jogada,
e de deteção de final de jogo
*/

% representação interna do GameState

/*
  0 1 2 3 4 
0 -----------
1 -----------
2 -----------
3 -----------


MOVE : Row-Column 0-0
translateCoords('a', 0), if needed


GameState : Board-Player
-> board
-> player a jogar
*/


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

          
test_correct_move3(NewGameState) :- board(5, Board), move(Board-1, 3-2-4-1, NewGameState).

player_cell(1, 1).
player_cell(2, 2).

opponent_player(1, 2).
opponent_player(2, 1).


% move(+GameState, , +Move, -NewGameState)

/*
um movimento é valido se e só se for uma casa vazia e resultar de um movimento em L
*/

cell_in_board(Row-Column) :-
    Row >= 0, Row < 9, Column >= 0, Column < 9.

empty_cell(Row-Column, Board) :- 
    cell_in_board(Row-Column),
    nth0(Row, Board, RowCell),
    nth0(Column, RowCell, Cell),
    Cell =:= 0.

belongs_to(Player, Row-Column, Board) :- 
    cell_in_board(Row-Column),
    nth0(Row, Board, RowCell),
    nth0(Column, RowCell, Cell),
    player_cell(Player, PlayerCell),
    Cell == PlayerCell.

% horse_move(+Row-Column, ?NewRow-NewColumn)
horse_move(Row-Column, NewRow-NewColumn) :-
    NewRow is Row-2, NewColumn is Column-1, cell_in_board(NewRow-NewColumn).
horse_move(Row-Column, NewRow-NewColumn) :-
    NewRow is Row-2, NewColumn is Column+1, cell_in_board(NewRow-NewColumn).
horse_move(Row-Column, NewRow-NewColumn) :-
    NewRow is Row+2, NewColumn is Column-1, cell_in_board(NewRow-NewColumn).
horse_move(Row-Column, NewRow-NewColumn) :-
    NewRow is Row+2, NewColumn is Column+1, cell_in_board(NewRow-NewColumn).
horse_move(Row-Column, NewRow-NewColumn) :-
    NewRow is Row-1, NewColumn is Column-2, cell_in_board(NewRow-NewColumn).
horse_move(Row-Column, NewRow-NewColumn) :-
    NewRow is Row-1, NewColumn is Column+2, cell_in_board(NewRow-NewColumn).
horse_move(Row-Column, NewRow-NewColumn) :-
    NewRow is Row+1, NewColumn is Column-2, cell_in_board(NewRow-NewColumn).
horse_move(Row-Column, NewRow-NewColumn) :-
    NewRow is Row+1, NewColumn is Column+2, cell_in_board(NewRow-NewColumn).


% é um horse move
% ou um conjunto de outras casas do inimigo que pode usar para chegar à final vazia

valid_move(_, Cell, EndCell, _) :-
    horse_move(Cell, EndCell).

valid_move(Player, Cell, EndCell, Board) :-
    horse_move(Cell, JumpCell),
    opponent_player(Player, OpponentPlayer),
    belongs_to(OpponentPlayer, JumpCell, Board),
    replace(JumpCell, -1, Board, AuxBoard),
    valid_move(Player, JumpCell, EndCell, AuxBoard).


replace(Row-Column, Value, Board, BoardResult) :- 
    nth0(Row, Board, BoardRow),
    nth0(Column, BoardRow, _, DeletedBoardRow),                 % delete this element in list
    nth0(Column, ReplacedBoardRow, Value, DeletedBoardRow),
    nth0(Row, Board, _, DeletedBoard),
    nth0(Row, BoardResult, ReplacedBoardRow, DeletedBoard).


make_move(Player, Cell, EndCell, Board, NewGameState) :-
    replace(Cell, 0, Board, Board1),
    player_cell(Player, PlayerCell),
    replace(EndCell, PlayerCell, Board1, NewBoard),
    opponent_player(Player, OpponentPlayer),
    NewGameState = NewBoard-OpponentPlayer.
    

move(Board-CurrentPlayer, Row-Col-EndRow-EndCol, NewGameState) :-
    belongs_to(CurrentPlayer, Row-Col, Board),
    empty_cell(EndRow-EndCol, Board),
    valid_move(CurrentPlayer, Row-Col, EndRow-EndCol, Board),
    make_move(CurrentPlayer, Row-Col, EndRow-EndCol, Board, NewGameState).






% game_over(+GameState, -Winner)

game_over(Board-CurrentPlayer, CurrentPlayer) :- 
    check_win(CurrentPlayer, Board).
game_over(Board-CurrentPlayer, OpponentPlayer) :- 
    opponent_player(CurrentPlayer, OpponentPlayer),
    check_win(OpponentPlayer, Board).

diff_1(Cell) :- Cell =\= 1.
diff_2(Cell) :- Cell =\= 2.

% 1 - ganho se as 2 primeiras linhas da reversa for o meu simbolo so e nenhum mais
check_win(1, Board) :- 
    reverse(Board, BoardReverse),
    nth0(0, BoardReverse, Row0),
    nth0(1, BoardReverse, Row1),
    append(Row0, Row1, Rows),
    \+ some(diff_1, Rows).


% 2 - ganho se as 2 primeiras linhas do board normal for o meu simbolo so e nada mais
check_win(2, Board) :- 
  nth0(0, Board, Row0),
  nth0(1, Board, Row1),
  append(Row0, Row1, Rows),
  \+ some(diff_2, Rows).






test_no_winner(Winner) :- board(0, Board), game_over(Board-1, Winner).
test_winner_1(Winner) :- board(1, Board), game_over(Board-1, Winner).
test_winner_2(Winner) :- board(2, Board), game_over(Board-1, Winner).

test_unvalid_move :- board(4, Board), valid_move(1, 4-3, 4-1, Board).
test_valid_move :- board(4, Board), valid_move(1, 4-3, 5-6, Board).
test_all_valid_moves(ValidMoves) :- board(4, Board), valid_move(1, 4-3, ValidMoves, Board).

test_correct_move1(NewGameState) :- board(4, Board), move(Board-1, 4-3-5-6, NewGameState).
test_correct_move2(NewGameState) :- board(4, Board), move(Board-2, 7-1-5-0, NewGameState).
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