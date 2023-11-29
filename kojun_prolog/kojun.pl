:- use_module(library(clpfd)).

/* Tabuleiro 10: https://www.janko.at/Raetsel/Kojun/010.a.htm 
Tabuleiro 2: https://www.janko.at/Raetsel/Kojun/002.a.htm */

tabuleiro(10, [ [(00-5), (01-_), (01-2), (01-_), (02-2), (02-_), (02-3), (02-1), (03-3), (03-1)],
               [(00-_), (00-4), (00-_), (01-1), (04-_), (04-5), (05-_), (05-5), (03-_), (05-4)],
               [(06-7), (06-5), (00-1), (04-7), (04-_), (07-_), (08-3), (05-1), (05-3), (05-_)],
               [(06-_), (06-4), (04-_), (04-_), (09-_), (07-_), (08-_), (08-_), (08-_), (10-3)],
               [(06-2), (06-_), (06-3), (04-4), (09-_), (09-2), (11-_), (10-_), (10-4), (10-_)],
               [(12-5), (12-_), (13-2), (13-_), (13-6), (09-_), (14-_), (14-_), (15-_), (15-_)],
               [(12-_), (12-1), (12-3), (13-_), (13-1), (16-_), (17-_), (18-4), (15-_), (15-3)],
               [(19-6), (19-7), (12-_), (13-3), (16-_), (16-1), (17-4), (18-_), (20-_), (20-1)],
               [(19-4), (19-_), (21-3), (21-_), (21-4), (21-_), (17-_), (18-_), (18-_), (22-3)],
               [(19-_), (19-1), (19-_), (21-2), (21-_), (21-6), (17-2), (17-_), (22-2), (22-1)]]).

% Verifica adjacências laterais ou horizontais.
% Basta transpor a matriz.
celulas_adjacentes([]).
celulas_adjacentes([_|[]]).
celulas_adjacentes([_-Adj_1,Regiao-Adj_2|Tail]) :-
    % Verifica se os valores nas laterais são diferentes.
    % Na matriz transposta verifica valores acima e abaixo.
    Adj_1 #\= Adj_2,
    % Recursividade até o final da linha/coluna
    celulas_adjacentes([Regiao-Adj_2|Tail]).

% Regra de valor inferior/superior
restricao_vertical([]).
restricao_vertical([_|[]]).
restricao_vertical([Regiao_sup-Valor_sup,Regiao_inf-Valor_inf|Tail]) :-
    % Regiao_sup diferente de Regiao_inf OU Valor_sup > Valor_inf
    (Regiao_sup #\= Regiao_inf) #\/ (Valor_sup #> Valor_inf),
    % Recursividade até o final da linha
    restricao_vertical([Regiao_inf-Valor_inf|Tail]).

maior_regiao(Tab, Regiao, Max) :-
    append(Tab, Tab_linha),     % Concatena todas as linhas do tabuleiro em uma única linha
    sort(Tab_linha, Tab_ordenado),      % Ordena em ordem crescente todos os itens da lista
    group_pairs_by_key(Tab_ordenado, Grupos),       % Agrupa os itens da lista ordenada de acordo com o valor da região.
    member(Regiao-Valor, Grupos),       % Organiza em tuplas (regiao, valor) para dada região.
    length(Valor, Max).        % valor máximo possível da região é o tamanho da lista de tuplas.

% limites(_, []). %condição de parada da recursão (acho que é desnecessário)
limites(Tab, [Regiao-Valor|[]]) :-      % Caso seja a última célula
    maior_regiao(Tab, Regiao, Max),
    Valor in 1..Max.
limites(Tab, [Regiao-Valor, Next|Tail]) :-      % Recursividade para buscar os limites da célula
    % maximo_regiao encontra a célula de maior valor da região.
    maior_regiao(Tab, Regiao, Max),
    % O intervalo será entre 1 e o valor encontrado por maximo_regiao.
    Valor in 1..Max,
    limites(Tab, [Next|Tail]).      % Aplicamos recursivamente para ter todos os limites.

% Verifica se não há repetições na lista de linha/coluna.
% Se não houver, retorna verdadeiro.
valores_unicos(_-List) :- all_distinct(List).
possibilidades_unicas(Tab) :-       % distinct verifica a regra de val. únicos. 
    append(Tab, Linha_unica),       % a matriz de Tabuleiro é transformada em uma linha só.
    sort(Linha_unica, Linha_ordenada),  % Ordena a linha obtida.
    group_pairs_by_key(Linha_ordenada, Grupos), % Separa os valores por região.
    maplist(valores_unicos, Grupos).    % Aplicamos val_unicos em todas as regiões.

% Obtém recursivamente os valores de dada região.
% H = tupla atual, T = cauda da lista, 
% H2 = lista de valores de retorno, T2 = Cauda da lista de retorno
valores_regiao([], []).     % Condição de parada da recursão
valores_regiao([H|T], [H2|T2]) :-  
    pairs_values(H, H2),    % Insere o valor de H em H2.
    valores_regiao(T, T2).

% Função de backtracking que irá "chutar" os valores possíveis.
testa_possibilidades(Linhas) :-
    valores_regiao(Linhas, Valores),    % Obtém uma lista de valores possíveis.
    % Aplica o predicado imbutido "label" na lista obtida
    % "chutando" uma solução válida para cada valor vazio da lista,
    % de modo que satisfaça as regras do jogo.
    maplist(label, Valores).

print_tabuleiro(Linhas) :- 
    valores_regiao(Linhas, Valores), 
    maplist(portray_clause, Valores).

% Aplicar as regras do jogo:
restricao_possibilidades(Linhas) :-
    maplist(limites(Linhas), Linhas),
    possibilidades_unicas(Linhas),
    maplist(celulas_adjacentes, Linhas),
    transpose(Linhas, Colunas),
    maplist(celulas_adjacentes, Colunas),
    maplist(restricao_vertical, Colunas).

solucionador(N) :-
    tabuleiro(N,Tab),   % Escolhe um tabuleiro.
    restricao_possibilidades(Tab),
    testa_possibilidades(Tab),      % Etapa de backtracking.
    print_tabuleiro(Tab). %exibe o tabuleiro resolvido.     % Exibir o resultado.