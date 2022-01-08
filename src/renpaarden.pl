:- consult('logic.pl').
:- consult('io.pl').


build_line(Value, Size, Line)  :- 
    length(Line, Size), 
    maplist(=(Value), Line).

build_lines(0, _, _, []):-!.
build_lines(N, Value, Size, [Line | T]) :-
    N > 0,
    build_line(Value, Size, Line),
    N1 is N-1,
    build_lines(N1, Value, Size, T).

% build_board(+Size, -Board)
build_board(Size, Board) :-
    build_line(1, Size, P1Line),
    build_line(2, Size, P2Line),
    MidLinesN is Size - 4,
    build_lines(MidLinesN, 0, Size, MidLines),
    append([P1Line, P1Line], MidLines, AuxBoard),
    append(AuxBoard, [P2Line, P2Line], Board).
  

% initial_state(+Size, -GameState)
initial_state(Size, Board-Player) :-
  build_board(Size, Board), 
  Player is 1.

/*
Problema do size global no logic.pl no cell_in_board

Ciclo de Jogo::
play :- ask_size, ask_opponet, initial_state, display_board, player_move
player_move :- ask_coord, move, game_over? (ciclo de player moves ate game over)
*/

/*
Renpaarden 

1 - Play against Human -> size -> play -> player moves contra humano
2 - Play against IA -> size -> play -> player moves player 2 = random move ia (choose moves de acordo com o nivel)
3 - Exit
*/
