:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

:- consult('logic.pl').
:- consult('io.pl').

:- dynamic size/1.
size(9).

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



init_board_size(Size) :-
    ask_board_size(Size),
    retractall(size(_)),
    asserta(size(Size)).

play :-
    show_initial_menu,
    ask_menu_option(Option),
    play(Option).

play(5) :- !.
play(1) :- 
    init_board_size(Size),
    initial_state(Size, GameState),
    display_game(GameState),
    game_cycle(GameState, human-0/human-0).

play(2) :- 
    init_board_size(Size),
    ask_ai_level(Level, 'IA Level [1-2]: '),
    initial_state(Size, GameState),
    display_game(GameState),
    game_cycle(GameState, human-0/pc-Level).

play(3) :- 
    init_board_size(Size),
    ask_ai_level(Level, 'IA Level [1-2]: '),
    initial_state(Size, GameState),
    display_game(GameState),
    game_cycle(GameState, pc-Level/human-0).

play(4) :- 
    init_board_size(Size),    
    ask_ai_level(Level1, 'IA1 Level [1-2]: '),
    ask_ai_level(Level2, 'IA2 Level [1-2]: '),
    initial_state(Size, GameState),
    display_game(GameState),
    game_cycle(GameState, pc-Level1/pc-Level2).



    
    
game_cycle(GameState, _) :-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(Board-1, P1-L1/P2-L2) :-
    display_turn(1),
    repeat,
    choose_move(Board-1, P1-L1, Move),
    move(Board-1, Move, NewGameState),
    display_game(NewGameState), !,
    game_cycle(NewGameState, P1-L1/P2-L2).

game_cycle(Board-2, P1-L1/P2-L2) :-
    display_turn(2),
    repeat,
    choose_move(Board-2, P2-L2, Move),
    move(Board-2, Move, NewGameState),
    display_game(NewGameState), !,
    game_cycle(NewGameState, P1-L1/P2-L2).

choose_move(_GameState, human-0, Row-Col-EndRow-EndCol) :-
    size(Size),
    ask_coord('Cell To Move [ex.: 2a]: ', Size, Row-Col),
    ask_coord('End Cell [ex.: 2a]: ', Size, EndRow-EndCol).
choose_move(GameState, pc-Level, Move) :-
    choose_move(GameState, Level, Move).
    

choose_move(GameState, 1, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).
choose_move(Board-Player, 2, Move) :-
    valid_moves(Board-Player, Moves),
    setof(Val-Mv, NewGameState^( member(Mv, Moves),
                    move(Board-Player, Mv, NewGameState),
                    value(NewGameState, Player, Val) ), ValueMoves),
    nth0(0, ValueMoves, MinVal-_M),
    min_value_moves(ValueMoves, MinVal, MinValueMoves),
    random_member(Move, MinValueMoves).
    
    % obter todos os moves de cujo menor valor é igual e dar um random desses 

min_value_moves([], _, []).  
min_value_moves([Val-Mv | T], Val, [Mv | R]) :-
    !, min_value_moves(T, Val, R).
min_value_moves(_, _, []). 
