:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

:- consult('logic.pl').
:- consult('io.pl').

:- dynamic size/1.

% size(?Size)
% Tamanho, por defeito, do tabuleiro quadrado.
size(9).




% play/0
% Predicado principal que dá acesso ao menu jogo atuando em conformidade com a opção escolhida.
play :-
    show_initial_menu,
    ask_menu_option(Option),
    play(Option).

% play(+Option)
% Opção de sair do Jogo
play(5) :- !.

% Human Vs Human - permite configurar os parametros necessários para iniciar o ciclo de jogo entre H/H.
play(1) :- 
    init_board_size(Size),
    initial_state(Size, GameState),
    display_game(GameState),
    game_cycle(GameState, human-0/human-0).

% Human Vs Pc - permite configurar os parametros necessários para iniciar o ciclo de jogo entre H/PC.
play(2) :- 
    init_board_size(Size),
    ask_ai_level(Level, 'IA Level [1-2]: '),
    initial_state(Size, GameState),
    display_game(GameState),
    game_cycle(GameState, human-0/pc-Level).

% Pc Vs Human - permite configurar os parametros necessários para iniciar o ciclo de jogo entre PC/H.
play(3) :- 
    init_board_size(Size),
    ask_ai_level(Level, 'IA Level [1-2]: '),
    initial_state(Size, GameState),
    display_game(GameState),
    game_cycle(GameState, pc-Level/human-0).

% Pc Vs Pc - permite configurar os parametros necessários para iniciar o ciclo de jogo entre PC/PC.
play(4) :- 
    init_board_size(Size),    
    ask_ai_level(Level1, 'IA1 Level [1-2]: '),
    ask_ai_level(Level2, 'IA2 Level [1-2]: '),
    initial_state(Size, GameState),
    display_game(GameState),
    game_cycle(GameState, pc-Level1/pc-Level2).



% game_cycle(+GameState, +GameMode)
% Jogador do turno correspondente ao GameState efetua uma jogada ou o jogo é terminado com a declaração do vencedor.
% Nos casos em que um jogador realiza um movimento este é pedido até corresponder a um movimento válido para o jogador do turno.
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
    


% choose_move(+GameState, +PlayerType, -Cell-EndCell)
% Escolha do movimento a ser realizado no caso de se tratar de um jogador humano.
% Pergunta ao utilizador as coordenadas da peça que pretende jogar e as coordenadas para onde pretende movê-la.
choose_move(_GameState, human-0, Row-Col-EndRow-EndCol) :-
    size(Size),
    ask_coord('Cell To Move [ex.: 2a]: ', Size, Row-Col),
    ask_coord('End Cell [ex.: 2a]: ', Size, EndRow-EndCol).

% Escolhe um movimento válido dependendo do nível da IA.
choose_move(GameState, pc-Level, Move) :-
    choose_move(GameState, Level, Move).
    

% choose_move(+GameState, +Level, -Move)
% Para o nível 1 devolve uma jogada válida aleatória.
choose_move(GameState, 1, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).

% Para o nível 2 avalia o tabuleiro resultante de cada jogada possível atribuindo-lhe um valor que é guardado.
% Quanto menor o valor melhor será a jogada. 
% Quando é obtida mais do que uma jogada possível de igual menor valor é devolvida uma destas de forma aleatória.
choose_move(Board-Player, 2, Move) :-
    valid_moves(Board-Player, Moves),
    setof(Val-Mv, NewGameState^( member(Mv, Moves),
                    move(Board-Player, Mv, NewGameState),
                    value(NewGameState, Player, Val) ), ValueMoves),
    nth0(0, ValueMoves, MinVal-_M),
    min_value_moves(ValueMoves, MinVal, MinValueMoves),
    random_member(Move, MinValueMoves).
    
% min_value_moves(+ValueMovesList, +MinVal, -MinValueMovesList)
% Devolve uma lista dos movimentos cujo valor do tabuleiro resultante é igual a um minimo obtido.
% A lista ValueMovesList é ordenada. 
min_value_moves([], _, []).  
min_value_moves([Val-Mv | T], Val, [Mv | R]) :-
    !, min_value_moves(T, Val, R).
min_value_moves(_, _, []). 



% init_board_size(+Size)
% Pergunta ao utilizador o tamanho do tabuleiro desejado e substitui o valor do tamanho do tabuleiro, por defeito, do facto size/1.
init_board_size(Size) :-
    ask_board_size(Size),
    retractall(size(_)),
    asserta(size(Size)).

% initial_state(+Size, -GameState)
% Configurar o GameState de forma a conter um tabuleiro no seu estado inicial de jogo e o turno 1 (Jogador 1 joga primeiro).
initial_state(Size, Board-Player) :-
  build_board(Size, Board), 
  Player is 1.

% build_board(+Size, -Board)
% Construir um tabuleiro do tamanho dado com as 2 primeiras linhas preenchidas com 1, 
% as 2 últimas linhas preenchidas com 2 e as restantes preenchidas com 0.
build_board(Size, Board) :-
    build_line(1, Size, P1Line),
    build_line(2, Size, P2Line),
    MidLinesN is Size - 4,
    build_lines(MidLinesN, 0, Size, MidLines),
    append([P1Line, P1Line], MidLines, AuxBoard),
    append(AuxBoard, [P2Line, P2Line], Board).
  
% build_lines(+NumberOfLines, +Value, +Size, -ListOfLines)
% Construir uma lista de N linhas todas com o mesmo valor e tamanho.
build_lines(0, _, _, []):-!.
build_lines(N, Value, Size, [Line | T]) :-
    N > 0,
    build_line(Value, Size, Line),
    N1 is N-1,
    build_lines(N1, Value, Size, T).

% build_line(+Value, +Size, -Line)
% Construir uma lista de elementos Value com tamanho Size.
build_line(Value, Size, Line)  :- 
    length(Line, Size), 
    maplist(=(Value), Line).
