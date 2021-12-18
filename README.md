# PFL_TP2_T7_Repaarden5

## Identificação

**Jogo** : Renpaarden

**Grupo** (T7_Repaarden5): 

| Número mecanográfico | Nome Completo                    | Contrinuição |
| :------------------- | :------------------------------- | :----------: |
| 201906852            | Henrique Ribeiro Nunes           | [ ] %        |
| 201905427            | Patricia do Carmo Nunes Oliveira | [ ] %        |


## Instalação e Execução

(incluir todos os passos necessários para correta execução do jogo em ambientes Linux e Windows (para além da instalação do SICStus Prolog 4.7);
Caso seja necessária alguma configuração (para além da instalação padrão do software), ou seja usada uma fonte diferente da fonte por omissão)

### Windows

AQUI

### Linux

AQUI


## Descrição do jogo

Renpaarden é um jogo de tabuleiro, jogado num quadrado 9x9. Cada jogador possui 18 pedras brancas ou pretas. O jogo começa com as 18 pedras de cada jogador distribuídas por duas linhas em lados opostos do tabuleiro. 

### Objetivo

O objetivo de cada jogador é atravessar o tabuleiro com cada uma das pedras e colocá-las nas posições originais das pedras do jogador adversário.

### Regras

Alternadamente, a cada turno, um jogador apenas pode mover uma pedra. O movimento desta pedra deverá ser em forma de **'L'**, semelhante ao movimento de um cavalo num jogo de xadrez, movendo 2 casa num determinado sentido ortogonal, seguido de 1 casas na direção perpendicular ao mesmo. Este movimento deverá terminar sempre numa casa vazia, diretamente, por um único movimento, ou a partir de saltos consecutivos usando, para isso, as pedras do adversário. 

![Movimentos_da_pedra](imgs/explanation_description.png)

*Legenda:*

a) Movimento simples de uma `pedra branca` a partir dos possíveis **'L'**

b) Movimento de uma `pedra preta` pelos possíveis **'L'** diretos e **'L'** que se obtêm pelo salto na pedra do branca

## Lógica do Jogo

(: descrever (não basta copiar código fonte) o projeto e implementação da lógica
do jogo em Prolog. O predicado de início de jogo deve ser ***play/0***. Esta secção deve ter
informação sobre os seguintes tópicos (até 2400 palavras no total):)

### 1. Representação interna do estado do jogo

(indicação de como representam o estado do
jogo, incluindo tabuleiro (tipicamente usando lista de listas com diferentes átomos para as
peças), jogador atual, e eventualmente peças capturadas e/ou ainda por jogar, ou outras
informações que possam ser necessárias (dependendo do jogo). Deve incluir exemplos da
representação em Prolog de estados de jogo inicial, intermédio e final, e indicação do
significado de cada átomo (ie., como representam as diferentes peças))

- representacao do tabuleiro + lista de listas + significado de cada atomo (0, 1, 2 - como representam as diferentes peças)
- representacao do jogador atual
- nao existe captura de peças e ja todas se encontram no tabuleiro desde o inicio do jogo
- uma jogada é definida por um casa de inicio do player e casa final em branco e o conjunto de saltos pelas peças do adversario conta como uma só jogada
- exemplis do estados de jogo inicial, intermédio e final

### 2. Visualização do estado de jogo

(descrição da implementação do predicado de visualização
do estado de jogo. Pode incluir informação sobre o sistema de menus criado, assim como
interação com o utilizador, incluindo formas de validação de entrada. O predicado de
visualização deverá chamar-se ***display_game(+GameState)***, recebendo o estado de jogo
atual (que inclui o jogador que efetuará a próxima jogada). Serão valorizadas visualizações
apelativas e intuitivas. Serão também valorizadas representações de estado de jogo e
implementação de predicados de visualização flexíveis, por exemplo, funcionando para
qualquer tamanho de tabuleiro, usando um predicado ***initial_state(+Size, -GameState)***
que recebe o tamanho do tabuleiro como argumento e devolve o estado inicial do jogo.)

### 3. Execução de Jogadas

(Validação e execução de uma jogada, obtendo o novo estado do
jogo. O predicado deve chamar-se ***move(+GameState, +Move, -NewGameState)***.)

- uma jogada é válida se e só se corresponder a uma casa vazia e for resultado de um movimento em L ou sucessivos L por saltos sobre as peças do inimigo

### 4. Final do Jogo 

(Verificação da situação de fim do jogo, com identificação do vencedor. O
predicado deve chamar-se ***game_over(+GameState, -Winner)***.)

- o jogo termina quando um jogador consegue pocionar as suas peças nas ultimas 2 filas do lado oposto ao lado do tabuleiro onde começou

### 5. Lista de Jogadas Válidas

(Obtenção de lista com jogadas possíveis. O predicado deve
chamar-se ***valid_moves(+GameState, -ListOfMoves)***.)

### 6. Avaliação do Estado do Jogo*

(Forma(s) de avaliação do estado do jogo do ponto de vista
de um jogador, quantificada através do predicado ***value(+GameState, +Player, -Value)***.)

### 7. Jogada do Computador*

(Escolha da jogada a efetuar pelo computador, dependendo do
nível de dificuldade, através de um predicado ***choose_move(+GameState, +Level, -Move)***.)

O nível 1 deverá devolver uma jogada válida aleatória. O nível 2 deverá devolver a melhor
jogada no momento (algoritmo míope), tendo em conta a avaliação do estado de jogo.


## Conclusões

(Conclusões do trabalho, (até 250 palavras))

### Known issues

(limitações do trabalho desenvolvido)

### Roadmap

(possíveis melhorias identificadas)


## Bibliografia

(Listagem de livros, artigos, páginas Web e outros recursos usados durante o
desenvolvimento do trabalho)


***NOTA*** : Pode ainda incluir uma ou mais imagens ilustrativas da execução do jogo, mostrando um estado
de jogo inicial, e possíveis estados intermédio e final (estes estados de jogo podem ser codificados
diretamente no ficheiro de código para esta demonstração da visualização do estado de jogo,
usando predicados semelhantes ao predicado initial_state/2).

