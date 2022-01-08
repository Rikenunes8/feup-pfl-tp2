cell_code(0, 32). 
cell_code(1, 111). 
cell_code(2, 120). 

% display_banner(+Title)
% 
display_banner(Size, Title) :- 
  S is Size*4 + 7,
  format('~t ~w ~t~*|~n~n', [Title, S]).

% display_header(+Size)
display_header(Size) :-
  write('     '),
  display_columns(Size, 97).

% display_columns(+Size, +Code)
display_columns(0, _):-!.
display_columns(Size, Code) :- Size > 0,
  put_code(Code), write('   '),
  S is Size-1, C is Code+1,
  display_columns(S, C).

% display_board(+Board)
display_board(Board) :-
  length(Board, Size),
  display_header(Size), nl,
  S is Size*4 + 4,
  write('   '), format('~`-t~*|~n', [S]),
  display_board_lines(Board, 1), nl, nl.

% display_board_lines(+Board, +LineNumber)
display_board_lines([], _).
display_board_lines([Line | T], N) :-
  write(' '), write(N), write(' |'), display_line(Line), nl,
  length(Line, Size), S is Size*4 + 4,
  write('   '), format('~`-t~*|~n', [S]),
  N1 is N + 1,
  display_board_lines(T, N1).

% display_line(+BoardLine)
display_line([]).
display_line([Cell | T]) :-
  write(' '), cell_code(Cell, Code),
  put_code(Code), write(' |'), 
  display_line(T).

% display_turn(+Player)
display_turn(Player) :- 
  cell_code(Player, Code), 
  format('Player\'s Turn: ~c ~n~n~n', [Code]).

% display_game(+GameState)
display_game(Board-Player) :-
  length(Board, Size), nl,
  display_banner(Size, 'Renpaarden'),
  display_board(Board),
  display_turn(Player).


:- use_module(library(between)).

% ask_coord(+Text, +Size, -Cell)
ask_coord(Text, Size, Row-Col) :- 
  repeat, 
  write(Text),
  get_code(C1), get_code(C2), 
  peek_code(Enter), skip_line, Enter = 10,
  Row is C1-49, Col is C2-97, Max is Size-1,
  between(0, Max, Row),
  between(0, Max, Col). 

board_size('min', 5).
board_size('max', 9).

% ask_board_size(-Size)
ask_board_size(Size) :-
  repeat, 
  board_size('min', Min), board_size('max', Max),
  write('Board Size ['), write(Min), write(' to '), write(Max), write(']: '),
  get_code(S), peek_code(Enter), skip_line, Enter = 10,
  Size is S-48,
  between(Min, Max, Size).

  