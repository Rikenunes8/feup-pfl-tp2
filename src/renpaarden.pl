:-use_module(library(lists)).


% player_cellValue(+Player, ?CellValue)
% Determina qual o código interno de representação de uma célula do jogador no tabuleiro.
player_cellValue(1, 1).
player_cellValue(2, 2).

% opponent_player(+Player, ?OpponentPlayer)
% Determina qual o código do oponente do jogador identificado em Player.
opponent_player(1, 2).
opponent_player(2, 1).



% cell_in_board(+Row-Col)
% Verifica se as coordenadas de uma célula se encontram dentro dos limites do tabuleiro de jogo (9x9).
cell_in_board(Row-Col) :-
    Row >= 0, Row < 9, Col >= 0, Col < 9.

% empty_cell(+Row-Col, +Board)
% Verifica se uma célula do tabuleiro nas coordenadas (Row, Col) está vazia, ou seja se está identificada com 0.
empty_cell(Row-Col, Board) :- 
    cell_in_board(Row-Col),
    nth0(Row, Board, RowCell),
    nth0(Col, RowCell, CellValue),
    CellValue =:= 0.

% belongs_to(?Player, ?Row-Col, +Board)
% Determina se uma célula do tabuleiro está ocupada pelo jogador, ou as casas todas ocupadas pelo jogador.
belongs_to(Player, Row-Col, Board) :- 
    cell_in_board(Row-Col),
    nth0(Row, Board, RowCell),
    nth0(Col, RowCell, CellValue),
    player_cellValue(Player, PlayerCellValue),
    CellValue == PlayerCellValue.



% horse_move(+Row-Col, ?NewRow-NewCol)
% Determina os movimentos em L a partir de uma célula que terminem dentro dos limites do tabuleiro.
horse_move(Row-Col, NewRow-NewCol) :-
    NewRow is Row-2, NewCol is Col-1, cell_in_board(NewRow-NewCol).
horse_move(Row-Col, NewRow-NewCol) :-
    NewRow is Row-2, NewCol is Col+1, cell_in_board(NewRow-NewCol).
horse_move(Row-Col, NewRow-NewCol) :-
    NewRow is Row+2, NewCol is Col-1, cell_in_board(NewRow-NewCol).
horse_move(Row-Col, NewRow-NewCol) :-
    NewRow is Row+2, NewCol is Col+1, cell_in_board(NewRow-NewCol).
horse_move(Row-Col, NewRow-NewCol) :-
    NewRow is Row-1, NewCol is Col-2, cell_in_board(NewRow-NewCol).
horse_move(Row-Col, NewRow-NewCol) :-
    NewRow is Row-1, NewCol is Col+2, cell_in_board(NewRow-NewCol).
horse_move(Row-Col, NewRow-NewCol) :-
    NewRow is Row+1, NewCol is Col-2, cell_in_board(NewRow-NewCol).
horse_move(Row-Col, NewRow-NewCol) :-
    NewRow is Row+1, NewCol is Col+2, cell_in_board(NewRow-NewCol).

% valid_move(+Player, +Cell, ?EndCell, +Board)
% Verifica se um movimento é válido (moviemento em L que termina numa célula vazia, 
% ou sucessivos movimentos em L que saltam através de células onde se encontram pedras do adversário).
valid_move(_, Cell, EndCell, Board) :-
    horse_move(Cell, EndCell),
    empty_cell(EndCell, Board).
valid_move(Player, Cell, EndCell, Board) :-
    horse_move(Cell, JumpCell),
    opponent_player(Player, OpponentPlayer),
    belongs_to(OpponentPlayer, JumpCell, Board),
    replace(JumpCell, -1, Board, AuxBoard),
    valid_move(Player, JumpCell, EndCell, AuxBoard).


% replace(+Row-Col, +Value, +Board, -BoardResult)
% Substitui o valor de uma célula do tabuleiro, eliminando o elemento a modificar 
% e posteriormente inserindo-o com o valor pretendido ao nível da linha e do tabuleiro.
replace(Row-Col, Value, Board, BoardResult) :- 
    nth0(Row, Board, BoardRow),                                
    nth0(Col, BoardRow, _, DeletedBoardRow),                 
    nth0(Col, ReplacedBoardRow, Value, DeletedBoardRow),     
    nth0(Row, Board, _, DeletedBoard),                          
    nth0(Row, BoardResult, ReplacedBoardRow, DeletedBoard).     

% make_move(+Player, +Cell, +EndCell, +Board, -NewGameState)
% Executa um movimento no tabuleiro, substituindo o valor da célula a mover por 0 
% e o valor da célula final pelo código que representa as células do jogador.
% O resultado é o estado de jogo atualizado.
make_move(Player, Cell, EndCell, Board, NewBoard-OpponentPlayer) :- 
    replace(Cell, 0, Board, AuxBoard),                          
    player_cellValue(Player, PlayerCellValue),                             
    replace(EndCell, PlayerCellValue, AuxBoard, NewBoard),           
    opponent_player(Player, OpponentPlayer).                     


% move(+GameState, +Move, -NewGameState)
% Validação e execução de uma jogada obtendo o novo estado do jogo.
move(Board-CurrentPlayer, Row-Col-EndRow-EndCol, NewGameState) :-
    belongs_to(CurrentPlayer, Row-Col, Board),
    empty_cell(EndRow-EndCol, Board),
    valid_move(CurrentPlayer, Row-Col, EndRow-EndCol, Board),
    make_move(CurrentPlayer, Row-Col, EndRow-EndCol, Board, NewGameState).



% diff_1(+CellValue)
% Verifica se o valor é diferente de 1.
diff_1(CellValue) :- CellValue =\= 1.

% diff_2(+CellValue)
% Verifica se o valor é diferente de 2.
diff_2(CellValue) :- CellValue =\= 2.


% check_win(?Player, +Board)
% Verifica se algum jogador tem todas as suas pedras no lado oposto do tabuleiro ao que começou.
% O jogador 1 ganha quando tem todas as suas pedras nas duas linhas inferiores do tabuleiro.
check_win(1, Board) :- 
    reverse(Board, BoardReverse),
    nth0(0, BoardReverse, Row0),
    nth0(1, BoardReverse, Row1),
    append(Row0, Row1, Rows),
    \+ some(diff_1, Rows).
% O jogador 2 ganha quando tem todas as suas pedras nas duas linhas superiores do tabuleiro.
check_win(2, Board) :- 
  nth0(0, Board, Row0),
  nth0(1, Board, Row1),
  append(Row0, Row1, Rows),
  \+ some(diff_2, Rows).


% game_over(+GameState, -Winner)
% Verificação da situação final do jogo, com determinação do vencedor.
game_over(Board-CurrentPlayer, CurrentPlayer) :- 
    check_win(CurrentPlayer, Board).
game_over(Board-CurrentPlayer, OpponentPlayer) :- 
    opponent_player(CurrentPlayer, OpponentPlayer),
    check_win(OpponentPlayer, Board).
