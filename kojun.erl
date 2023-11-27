-module(kojun). 
-export([start/0]). 

%   Função auxiliar. Substitui o valor na coluna da linha selecionada na função atualizaTabuleiro.
atualizaColuna(Lista,Indice,Valor)->
    Lista_enumerada = lists:zip(lists:seq(1,length(Lista)), Lista), 
    [if I == Indice -> Valor; true -> Val end || {I, Val} <- Lista_enumerada].

%   Recebe a matriz de valores, coordenadas e o valor novo. Seleciona a linha da matriz e passa ela e o valor Y para a função auxiliar executar a substituição real
atualizaTabuleiro(Tab,{X,Y},Valor_novo)->
    Linha = lists:nth(X,Tab),
    Nova_Linha = atualizaColuna(Linha,Y,Valor_novo),
    atualizaColuna(Tab,X,Nova_Linha).

%   Retorna a quantidade de células com o valor de região presente na coordenada {X,Y}
tamanhoRegiao(Reg, {X, Y}) ->
  Value = valorCoordenada(Reg, {X,Y}),
  Count = length([C || R <- Reg, C <- R, C =:= Value]),
  Count.

%   Retorna o valor presente na coordenada. Função auxiliar para descobrir números da matriz de valores ou IDs de região na matriz de regiões.
valorCoordenada(Matriz, {X,Y}) -> 
    lists:nth(Y, lists:nth(X, Matriz)).

%   Retorna lista de coordenadas cujo valor na matriz de valores seja igual a 0
coordenadasZero(Tab)->
    Lim = length(Tab),
    Coordenada = [{Row, Col} || Row <- lists:seq(1, Lim), Col <- lists:seq(1, Lim)],
    Coords_zero = lists:filter(fun({X,Y})->valorCoordenada(Tab,{X,Y}) == 0 end, Coordenada),
    Coords_zero.

%   Retorna uma lista de valores já presentes na região
valoresRegiao(Tab, Reg, R) ->
    Coordenada = coordenadasRegiao(Reg, R),
    [lists:nth(J, lists:nth(I, Tab)) || {I,J} <- Coordenada].

%   Retorna a lista de coordenadas que possuem o ID de região informado
coordenadasRegiao(Reg, Reg_id) ->
    Rows = length(Reg),
    Cols = length(lists:nth(1, Reg)),
    Coordenada = [{X,Y} || X <- lists:seq(1, Rows), Y <- lists:seq(1, Cols)],
    lists:filter(fun({X,Y}) -> valorCoordenada(Reg, {X,Y}) == Reg_id end, Coordenada).

%   Retorna uma lista de valores adjacentes da coordenada informada.
celulasAdjacentes(Tab,{X,Y}) -> 
    Acima = {X-1,Y},
    Baixo = {X+1,Y},
    Esquerda = {X,Y-1},
    Direita = {X,Y+1},
    if
        (X>1)->
            Up = valorCoordenada(Tab,Acima);
        true->      % tratamento de coordenada localizada em borda superior da matriz
            Up = 0
    end,

    if
        (X < length(Tab))->
            Down = valorCoordenada(Tab,Baixo);
        true->      % tratamento de coordenada localizada em borda inferior da matriz
            Down = 0
    end,

    if 
        (Y < length(Tab))->
            Right = valorCoordenada(Tab,Direita);
        true->      % tratamento de coordenada localizada em borda direita da matriz
            Right = 0
    end,

    if 
        (Y>1)->
            Left = valorCoordenada(Tab, Esquerda);
        true->      % tratamento de coordenada localizada em borda esquerda da matriz
            Left = 0 
    end,

    Resultado = [Up,Down,Left,Right],
    Resultado.

%   Filtra lista de forma a só possuir valores maiores que o valor da célula abaixo da coordenada informada.
restricaoVerticalCima(Tab,Reg,{X,Y},Prefiltro)->
    if 
        (X == length(Tab))->    % tratamento borda inferior (não está acima de ninguém)
            Resultado = Prefiltro;
        true->
            Baixo = {X+1,Y},
            Regiao_id = valorCoordenada(Reg,{X,Y}),
            Regiao_id_abaixo = valorCoordenada(Reg,Baixo),
            Valor_abaixo = valorCoordenada(Tab,Baixo),
            if
                (Regiao_id_abaixo /= Regiao_id)->
                    Resultado = Prefiltro;
                true->
                    Resultado = lists:filter(fun(A)-> A > Valor_abaixo end, Prefiltro)
            end
    end,
    Resultado.

%   Filtra lista de forma a só possuir valores menores que o valor da célula acima da coordenada informada.
restricaoVerticalBaixo(Tab,Reg,{X,Y},Prefiltro)->
    if 
        (X == 1)->      % tratamento de borda superior (não está abaixo de ninguém)
            Resultado = Prefiltro;
        true->
            Acima = {X-1,Y},
            Regiao_id = valorCoordenada(Reg,{X,Y}),
            Regiao_id_acima = valorCoordenada(Reg,Acima),
            Valor_acima = valorCoordenada(Tab,Acima),
            if
                Regiao_id_acima /= Regiao_id orelse Valor_acima == 0 ->
                    Resultado = Prefiltro;
                true->
                    Resultado = lists:filter(fun(A)-> A < Valor_acima end, Prefiltro)
            end
    end,
    Resultado.

%   Função unificadora para evitar ter que fazer duas chamadas na função restricoesPossibilidades.
filtraRestricoesVerticals(Tab,Reg,{X,Y},Prefiltro)->
    FiltradoVertical1 = restricaoVerticalCima(Tab,Reg,{X,Y},Prefiltro),
    restricaoVerticalBaixo(Tab,Reg,{X,Y},FiltradoVertical1).

%   Caso a célula esteja localizada na posição mais baixa de sua região (menor) e não possua vizinhos horizontais da mesma região (solitário),
%   retorna True
menorSolitarioReg(Reg,{X,Y})->
    Regiao_id = valorCoordenada(Reg,{X,Y}),
    if
        (X == length(Reg))->
            Regiao_id_abaixo = -1;
        true->
            Baixo = {X+1,Y},
            Regiao_id_abaixo = valorCoordenada(Reg,Baixo)
    end, 
    Linha = lists:nth(X,Reg),
    Num_mesma_regiao_linha = length(lists:filter(fun(A)-> A==Regiao_id end, Linha)),
    if
        (Num_mesma_regiao_linha == 1 andalso Regiao_id /= Regiao_id_abaixo) ->
            true;
        true->
            false
    end.

%   Caso a célula esteja localiza na posição mais alta de sua região (maior) e não possua vizinhos horizontais da mesma região (solitário),
%   retorna True
maiorSolitarioReg(Reg,{X,Y})->
    Regiao_id = valorCoordenada(Reg,{X,Y}),
    if
        (X == 1)->
            Regiao_id_acima = -1;
        true->
            Acima = {X-1,Y},
            Regiao_id_acima = valorCoordenada(Reg,Acima)
    end, 
    Linha = lists:nth(X,Reg),
    Num_mesma_regiao_linha = length(lists:filter(fun(A)-> A==Regiao_id end, Linha)),
    if
        (Num_mesma_regiao_linha == 1 andalso Regiao_id /= Regiao_id_acima) ->
            true;
        true->
            false
    end.

%   De acordo com o resultado das funções "solitário" realiza a filtragem adequada. 
%   Mas apenas se a lista de possibilidades for > 1.
filtraSolitarioRegiao(Reg, {X,Y}, Prefiltro) ->
    Tamanho_Prefiltro = length(Prefiltro),
    IsLowestLonely = menorSolitarioReg(Reg,{X,Y}),
    IsHighestLonely = maiorSolitarioReg(Reg,{X,Y}),
    if 
        Tamanho_Prefiltro > 1 andalso IsLowestLonely ->     % Caso seja o menor solitário da região...
            Valor_max = lists:max(Prefiltro), 
            lists:subtract(Prefiltro, [Valor_max]);     %..Remove o maior valor da lista de possibilidades, pois a célula mais baixa não pode possuir o maior valor.
        Tamanho_Prefiltro > 1 andalso IsHighestLonely ->    % Caso seja o maior solitário da região...
            Valor_min = lists:min(Prefiltro),
            lists:subtract(Prefiltro, [Valor_min]);     %...Remove o menor valor da lista de possibilidades, pois a célula mais alta não pode possuir o menor valor.
        true ->
            Prefiltro   % Caso não seja um solitário ou só possua uma possibilidade, não aplica nenhum filtro.
    end.

%   Retorna uma lista de todas as possibilidades para uma dada coordenada.
restricaoPossibilidades(Tab, Reg, {X,Y}) ->
    Reg_id = valorCoordenada(Reg,{X,Y}),
    %   Lista de possibilidades inicia como uma lista com todos os numeros de 1 até (tamanho da região):
    PossibilidadesBasicas = lists:seq(1, tamanhoRegiao(Reg, {X,Y})), 
    %   São removidos da lista os valores já presentes na região:
    PossibilidadesReg = lists:subtract(PossibilidadesBasicas, valoresRegiao(Tab, Reg,Reg_id)), 
    %   São removidos da lista valores presentes em células adjacentes:
    PossibilidadesAdj = lists:subtract(PossibilidadesReg, celulasAdjacentes(Tab,{X,Y})),
    %   Valores são removidos da lista de acordo com as restrições verticais (maior que inferior, menor que superior):
    PossibilidadesVert = filtraRestricoesVerticals(Tab,Reg,{X,Y},PossibilidadesAdj),
    %   Valores são removidos da lista de acordo com os resultados das funçoes p. solitários.
    Final = filtraSolitarioRegiao(Reg,{X,Y},PossibilidadesVert), 
    Final.

%   Chama valoresUnicos e realiza a atualização da matriz de células caso existam valoresUnicos.
possibilidadesUnicas(Tab,Possibilidades_Posicoes)->
    Coords_e_valores_unicos = valoresUnicos(Possibilidades_Posicoes),
    if
        Coords_e_valores_unicos == [] ->
            Tab;
        true->
            atualizaValoresComUnicos(Tab,Coords_e_valores_unicos)
    end.

%   Função chamada pelo pré-solucionador. Se encarrega de inicializar o contador da chamada recursiva com o maior valor de ID de região.
encontrarUnicaPossibilidade(Tab,Reg)->
    Comeco_Count = lists:max(lists:max(Reg)),
    encontrarUnicaPossibilidade(Tab,Reg,Comeco_Count).

%   Vasculha o tabuleiro em busca de algum valor que aparece apenas em um lugar da região. Por exemplo:
encontrarUnicaPossibilidade(Tab,Reg,Count)->    %  Matriz de valores, matriz de regiões, ID da região.
    Possibilidades = valoresPossiveisDaRegiao(Tab,Reg,Count),   % Digamos que a lista de valores possiveis da região retornou [[1,2],[1,2,3],[1,3],[2,3,4]]
    Novo_tab = possibilidadesUnicas(Tab,Possibilidades),    %  Ele encontrará que o valor 4 só ocorre em uma das possibilidades da região, então é inserido lá.
    if
        Count == 1->  % Quando chega no final da chamada recursiva (Count ==1)
            Novo_tab;  % retorna o Novo_tab
        true->  % Caso contrário,
            encontrarUnicaPossibilidade(Novo_tab,Reg,Count-1)   % procura outro valor na próxima região (de ID mais alto para o mais baixo)
    end.

%   função auxiliar e recursiva para atualizar a tabela com valores unicos.
atualizaValoresComUnicos(Tab,[{{X,Y},Valor} | Resto])->
    Novo_tab = atualizaTabuleiro(Tab,{X,Y},hd(Valor)),
    if
        Resto == [] ->      % indica fim das chamadas recursivas / chegou no final da lista.
            Novo_tab;
        true->
            atualizaValoresComUnicos(Novo_tab,Resto)
    end.

%   Verifica se na lista de tuplas coordendas,possibilidades existe algum valor de possibilidade que só apareça em uma coordenada 
%   e retornará as coordenadas que possuem valores unicos.
%   ex: [((1,0),[1,2]),((1,1),[1,3]),((1,2),[1,2]) retorna [(1,1),3]
valoresUnicos(Lista_Coords_Poss)->
    Lista_Possibilidades = lists:flatten([Poss || {_, Poss} <- Lista_Coords_Poss]),
    Lista_Unicos = 
        lists:filter(fun(A) -> 
        length(lists:filter(fun(B) -> B == A end, Lista_Possibilidades)) == 1 end, Lista_Possibilidades),
    Res = coordsValoresUnicos(Lista_Coords_Poss,Lista_Unicos),
    Res.

%   função auxiliar para valoresUnicos.
coordsValoresUnicos(Lista_Coords_Poss, Lista_Unicos) ->
    Lista = lists:map(fun({{X, Y}, List}) ->
        Unicos = lists:filter(fun(Value) ->
            lists:member(Value, List)
        end, Lista_Unicos),
        if length(Unicos) == 1 ->
            {{X, Y}, Unicos};
           true -> {}
        end
    end, Lista_Coords_Poss),
    lists:filter(fun(A)-> A /= {} end, Lista).

%   Recebe uma lista com todas as coordenadas com valor == 0 e retorna uma matriz de possibilidades onde cada linha é a possibilidade de uma das coordenadas.
possibilidadesVazios(_,_,[],Resultado)->    %   indica fim da chamada recursiva / final da lista de coordenadas
    Resultado;
possibilidadesVazios(Tab,Reg,[{X,Y} | Resto],Resultado)->
    Possibilidades_atual= restricaoPossibilidades(Tab,Reg,{X,Y}),
    Outras_possibilidades = possibilidadesVazios(Tab,Reg,Resto,Resultado),
    Resultado ++ [Possibilidades_atual] ++ Outras_possibilidades.

%   Função principal do Backtracking.
testaPossibilidades(Tab,_,_,[])->   % final da chamada recursiva / fim da lista de possibilidades
    Tab;
testaPossibilidades(Tab,Reg,{X,Y}, [Possibilidade | Resto])->
    %   Cria uma matriz de valores com uma possibilidade 'chutada':
    Novo_tab = atualizaTabuleiro(Tab,{X,Y},Possibilidade), 
    Coords_zero = coordenadasZero(Tab),     % coordenadas vazias da matriz de valores original
    Poss_vazios = possibilidadesVazios(Tab,Reg,Coords_zero,[]),     % possibilidades das coordenadas vazias da matriz de valores original
    Novo_Coords_zero = coordenadasZero(Novo_tab),      % coordenadas vazias da nova matriz
    Novo_Poss_vazios = possibilidadesVazios(Novo_tab,Reg,Novo_Coords_zero,[]),      % possibilidades das coordenadas vazias da nova matriz
    Tamanho_Novo_Poss = length(Novo_Poss_vazios),       % numero de linhas na matriz de possibilidades nova
    Tamanho_Poss = length(Poss_vazios),     % numero de linhas na matriz de possibilidades antiga
    if
        % Se o número de célula com possibilidades diminui em mais de um ao preencher
        %  uma célula, isso significa que o preenchimento desta célula com o valor X
        % impossibilita que qualquer valor seja inserido em outra célula, ou seja,
        %   X é o valor errado.
        Tamanho_Novo_Poss < (Tamanho_Poss - 1) ->
            testaPossibilidades(Tab,Reg,{X,Y},Resto);   % chuta o proximo numero na lista de possibilidades da coordenada
        true ->
            solucionador(Novo_tab,Reg)      % Retorna ao solucionador com a matriz atualizada com o valor chutado.
    end.


%   Retorna uma lista de tuplas de coordenadas e valores possiveis para dadas coordenadas de uma região
valoresPossiveisDaRegiao(Tab,Reg,Reg_id)->
    Coords = coordenadasRegiao(Reg,Reg_id),
    Coords_zeros = lists:filter(fun({X,Y})-> valorCoordenada(Tab, {X,Y}) == 0 end, Coords),
    Lista_resultado = [{Coordenada,restricaoPossibilidades(Tab,Reg,Coordenada)} || Coordenada <- Coords_zeros],
    Lista_resultado.

%   Função auxiliar de solucionador. Aplica todas as regras e deduções implementadas
preSolucionador(Tab, Reg, [{X,Y} | Resto])->
    %   Cria um novo tabuleiro com valores unicos (caso não existam nenhum, irá retornar o mesmo tabuleiro que o informado)
    Novo_tab1 = encontrarUnicaPossibilidade(Tab,Reg), 
    Possibilidades = restricaoPossibilidades(Novo_tab1,Reg,{X,Y}),      % retorna lista de possibilidades da coordenada
    
    if 
        length(Possibilidades) == 1 ->      % Caso só exista uma possibilidade para dada coordenada
            Novo_Tab2 = atualizaTabuleiro(Novo_tab1,{X,Y},hd(Possibilidades));      % Insere o valor na matriz
        true->
            Novo_Tab2 = Novo_tab1
    end,
    if
        Resto == [] ->      %  Caso tenha chegado ao final da lista de coordendas informada
            Novo_Tab2;      % retorna o tabuleiro
        true->            
            preSolucionador(Novo_Tab2, Reg, Resto)      % chamada recursiva de preSolucionador.
    end.

%   Função principal para solucionar o tabuleiro:
solucionador(Tab,Reg)->
    Coords_zero = coordenadasZero(Tab),     %  Pega a lista de coordenadas com valor 0 na matriz de valores
    if
        length(Coords_zero) == 0 ->     % caso não existam mais valores 0 o tabuleiro foi resolvido e o retorna
            Tab;
        true->
            Novo_tab = preSolucionador(Tab,Reg,Coords_zero),        % chama preSolucionador   
            if
                Novo_tab == Tab ->      % caso a aplicação de preSolucionador não tenha resultado em mudanças:
                    Novo_Coords_zero = coordenadasZero(Novo_tab),       % pega as novas coordenadas com valor 0
                    if length(Novo_Coords_zero) == 0 ->     % Se o tabuleiro está resolvido, devolve
                        Novo_tab;
                        true->  %   Caso não esteja resolvido ainda vai chutar valor e tentar de novo
                            Novo_poss = restricaoPossibilidades(Tab,Reg,hd(Novo_Coords_zero)),
                            testaPossibilidades(Tab,Reg,hd(Novo_Coords_zero), Novo_poss)
                    end;
                true->
                    solucionador(Novo_tab,Reg)
            end
    end.

%   função para mostrar o tabuleiro final em tela
printTabuleiro(Matriz) ->
    Num_zeros = length(coordenadasZero(Matriz)),
    if 
        Num_zeros > 0 ->
            io:format("TABULEIRO MAL FORMULADO. N° SOLUCOES /= 1 ");
        true->
        lists:foreach(
            fun(Row) -> 
                io:format("~p~n", [Row])
            end,
            Matriz
        )
    end.


start() -> 

    CelulasTab2 = [
        [0, 0, 4, 0, 2, 0],
        [0, 0, 3, 0, 0, 0],
        [1, 4, 0, 4, 0, 0],
        [0, 5, 0, 0, 0, 2],
        [0, 0, 0, 0, 3, 0],
        [6, 2, 0, 2, 0, 5]],

    RegioesTab2 = [
        [1, 2, 2, 2, 3, 4],
        [1, 5, 2, 3, 3, 3],
        [1, 1, 6, 3, 7, 7],
        [8, 10, 6, 9, 9, 7],
        [8, 10, 10, 11, 11, 7],
        [10, 10, 10, 11, 11, 11]],

CelulasTab10 = [
                        [5,0,2,0,2,0,3,1,3,1],
                        [0,4,0,1,0,5,0,5,0,4],
                        [7,5,1,7,0,0,3,1,3,0],
                        [0,4,0,0,0,0,0,0,0,3],
                        [2,0,3,4,0,2,0,0,4,0],
                        [5,0,2,0,6,0,0,0,0,0],
                        [0,1,3,0,1,0,0,4,0,3],
                        [6,7,0,3,0,1,4,0,0,1],
                        [4,0,3,0,4,0,0,0,0,3],
                        [0,1,0,2,0,6,2,0,2,1]],

RegioesTab10 = [
                        [1,2,2,2,3,3,3,3,4,4],
                        [1,1,1,2,6,6,7,7,4,7],
                        [5,5,1,6,6,9,8,7,7,7],
                        [5,5,6,6,10,9,8,8,8,11],
                        [5,5,5,6,10,10,30,11,11,11],
                        [12,12,15,15,15,10,22,22,21,21],
                        [12,12,12,15,15,16,17,18,21,21],
                        [13,13,12,15,16,16,17,18,20,20],
                        [13,13,14,14,14,14,17,18,18,19],
                        [13,13,13,14,14,14,17,17,19,19]],


    printTabuleiro(solucionador(CelulasTab10,RegioesTab10)).
