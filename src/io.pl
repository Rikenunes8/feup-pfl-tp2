
% cell_code(?CellValue, ?Code)
% Tradução do valor de uma célula para o código ASCII decimal da sua representação no ecrã.
cell_code(0, 32).
cell_code(1, 111).
cell_code(2, 120).


% display_banner(+Title)
% Visualização do título do jogo centrado com o tabuleiro.
display_banner(Size, Title) :- 
  S is Size*4 + 7,
  nl, format('~t ~w ~t~*|~n~n', [Title, S]).


% show_initial_menu/0
% Visualização do menu principal incluindo as suas opções disponíveis.
show_initial_menu :-
  display_banner(4, 'Renpaarden'),
  write('1 - Human Vs Human'), nl,
  write('2 - Human Vs Computer'), nl, 
  write('3 - Computer Vs Human'), nl, 
  write('4 - Computer Vs Computer'), nl,
  write('5 - Exit'), nl, nl.

% ask_menu_option(Option)
% Pergunta ao utilizador a opção que pretende escolher do menu inicial até esta ser válida.
ask_menu_option(Option) :-
  repeat,
  write('Option: '),
  get_code(Opt), peek_code(Enter), skip_line, Enter = 10,
  Option is Opt-48,
  between(1, 5, Option), !.


% board_size(?Limit, ?Value)
% Faz corresponder o tamanho limite do tabuleiro a um limite (min/max).
board_size(min, 5).
board_size(max, 9).

% ask_board_size(-Size)
% Pergunta ao utilizador o tamanho desejado para o tabuleiro até obter uma resposta dentro dos limites de tamanho de um tabuleiro.
ask_board_size(Size) :-
  repeat, 
  board_size(min, Min), board_size(max, Max),
  write('Board Size ['), write(Min), write(' to '), write(Max), write(']: '),
  get_code(S), peek_code(Enter), skip_line, Enter = 10,
  Size is S-48,
  between(Min, Max, Size), !.


% ask_ai_level(-Level, +Text)
% Pergunta ao utilizador o nível da IA até obter como resposta o valor 1 ou 2.
ask_ai_level(Level, Text) :-
  repeat,
  write(Text),
  get_code(L), peek_code(Enter), skip_line, Enter = 10,
  Level is L-48,
  between(1, 2, Level), !.


% display_game(+GameState)
% Visualização do jogo - título e respetivo tabuleiro.
display_game(Board-_) :-
  length(Board, Size), nl,
  display_banner(Size, 'Renpaarden'),
  display_board(Board).


% display_board(+Board)
% Visulaização do tabuleiro.
display_board(Board) :-
  length(Board, Size),
  display_header(Size), nl,
  S is Size*4 + 4,
  write('   '), format('~`-t~*|~n', [S]),
  display_board_lines(Board, 1), nl, nl.


% display_header(+Size)
% Visualização do cabeçalho do tabuleiro (numeração das colunas).
display_header(Size) :-
  write('     '),
  display_columns(Size, 97).

% display_columns(+Size, +Code)
% Computação e visualização das colunas do tabuleiro de acordo com o seu tamanho.
display_columns(0, _):-!.
display_columns(Size, Code) :- Size > 0,
  put_code(Code), write('   '),
  S is Size-1, C is Code+1,
  display_columns(S, C).


% display_board_lines(+Board, +LineNumber)
% Visualização das linhas que compõe o tabuleiro.
display_board_lines([], _).
display_board_lines([Line | T], N) :-
  write(' '), write(N), write(' |'), display_line(Line), nl,
  length(Line, Size), S is Size*4 + 4,
  write('   '), format('~`-t~*|~n', [S]),
  N1 is N + 1,
  display_board_lines(T, N1).

% display_line(+BoardLine)
% Visualização de uma linha do tabuleiro.
display_line([]).
display_line([Cell | T]) :-
  write(' '), cell_code(Cell, Code),
  put_code(Code), write(' |'), 
  display_line(T).


% display_turn(+Player)
% Visualização do código do jogador do turno atual.
display_turn(Player) :- 
  cell_code(Player, Code), 
  format('Player\'s Turn: ~c ~n~n~n', [Code]).



% ask_coord(+Text, +Size, -Cell)
% Pergunta ao utilizador as coordenadas de uma célula do tabuleiro até obter uma resposta dentro dos limites do tamanho do tabuleiro.
ask_coord(Text, Size, Row-Col) :- 
  repeat, 
  write(Text),
  get_code(C1), get_code(C2),
  peek_code(Enter), skip_line, Enter = 10,
  Row is C1-49, Col is C2-97, Max is Size-1,
  between(0, Max, Row),
  between(0, Max, Col), !.


% congratulate(+Winner)
% Congratula o vencedor do jogo mostrando uma mensagem bonita.
congratulate(Winner) :-
  format('+~`-t~*|+~n', [43]),
  write('| Congratulations to our WINNER!! Player '), cell_code(Winner, Code), put_code(Code), write(' |'), nl,
  format('+~`-t~*|+~n~n', [43]).
