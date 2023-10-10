import Data.List (nub, intersect, (\\))


type Regiao = [[Int]]
type Tabuleiro = [[Int]]

{-
Tabuleiro/Regiões 2: https://www.janko.at/Raetsel/Kojun/002.a.htm
Tabuleiro/Regiões 13: https://www.janko.at/Raetsel/Kojun/013.a.htm
-}

tabuleiro2::Tabuleiro
tabuleiro2=[
     [0,0,4,0,2,0],
     [0,0,3,0,0,0],
     [1,4,0,4,0,0],
     [0,5,0,0,0,2],
     [0,0,0,0,3,0],
     [6,2,0,2,0,5]]



regioes2::Regiao
regioes2=[
    [1, 2, 2, 2 , 3, 4],
    [1, 5, 2, 3, 3, 3],
    [1, 1, 6, 3, 7, 7],
    [8, 10, 6, 9, 9, 7],
    [8, 10, 10, 11,11,7],
    [10,10,10,11,11,11]]

tabuleiro13::Tabuleiro
tabuleiro13 =
                       [[4,0,1,0,6,0,0,3,0,0],
                        [0,5,2,1,5,0,4,0,3,0],
                        [0,0,7,0,0,0,3,0,0,2],
                        [0,1,0,0,0,0,0,6,0,1],
                        [0,0,3,6,0,0,3,0,4,0],
                        [3,6,0,4,0,0,0,0,0,3],
                        [0,4,0,0,0,6,0,5,0,4],
                        [1,0,0,0,1,0,0,3,5,0],
                        [0,2,0,4,0,0,0,0,2,7],
                        [0,6,7,0,0,4,0,2,0,0]]

regioes13::Regiao
regioes13=
                        [
                        [ 1, 1, 2, 2, 3, 3, 3, 3, 4, 5],
                        [ 1, 1, 3, 3, 3, 6, 7, 7, 8, 5],
                        [ 9, 1, 1, 1,10, 7, 7, 8, 8, 5],
                        [ 9, 9,11,11,10,12, 7, 5, 5, 5],
                        [13,11,11,14,14,12,12,12,15,15],
                        [13,11,11,14,14,17,12,15,15,15],
                        [13,11,16,14,14,17,17,17,18,18],
                        [13,13,16,16,17,17,19,20,18,21],
                        [22,22,23,16,24,17,20,20,18,18],
                        [22,16,16,16,24,24,24,24,18,18]]

-- Retorna uma lista com os números presentes na região informada
valoresRegiao :: Tabuleiro -> Regiao -> Int -> [Int]
valoresRegiao tab reg r = [tab !! i !! j | (i,j) <- coordsRegiao reg r]
    where
        coordsRegiao reg r = [(i,j) | i <- [0..n-1], j <- [0..n-1], reg !! i !! j == r]
        n = length tab

-- Determinar os valores das celulas adjacentes à cel. fornecida
-- up,down,left,right evitam index out of bound ao checar celulas presentes nas bordas
celulasAdjacentes :: Tabuleiro -> (Int, Int) -> [Int]
celulasAdjacentes matrix (row, col) =
  let numRows = length matrix
      numCols = length (matrix !! 0)
      up = if row == 0 then 0 else  (matrix !! (row - 1) !! col) --celula acima
      down = if row == numRows - 1 then 0 else  (matrix !! (row + 1) !! col) --celula abaixo
      left = if col == 0 then 0 else  (matrix !! row !! (col - 1)) --celula a esquerda
      right = if col == numCols - 1 then 0 else  (matrix !! row !! (col + 1)) -- celula a direita
  in nub [up, down, left, right]

-- Verificar todos os valores possíveis que uma coordenada pode assumir, conforme regras do Kojun
restricaoPossibilidades::Tabuleiro->Regiao->(Int,Int)-> [Int] 
restricaoPossibilidades tab reg (x,y)= do
    let possibilidades_basicas = [1..tamanhoRegiao reg (x,y)] -- Qualquer numero entre 1 e tamanho da regiao
    let possibilidades_reg =possibilidades_basicas \\ valoresRegiao tab reg (reg!!x!!y) -- Remove itens que já estão presentes na regiao
    let possibilidades_adj  = possibilidades_reg \\ (celulasAdjacentes tab (x,y)) -- Remove itens que já estão presentes em alguma adjacencia
    let possibilidades_vert = filtraRestricoesVerticais tab reg (x,y) possibilidades_adj --Remove de acordo com as restrições verticais (maiores que a celula acima ou menores que a celula abaixo)
    filtraSolitarioRegiao tab reg (x,y) possibilidades_vert -- Caso a coordenada esteja na posição mais alta/baixa da região, remove o item menor/maior

-- Função auxiliar para filtrar valores maiores que o da célula na coordenada acima de (x,y) da mesma região
restricaoVerticalCima :: Tabuleiro -> Regiao -> (Int, Int) -> [Int]
restricaoVerticalCima tab reg (x, y)
    | x == length tab-1 = [] --evita index out of bounds. Itens da ultima linha não tem que se preocupar em ser maiores que o numero abaixo
    | reg!!(x+1)!!y /= id_regiao = [] --nao faz nada se a celula abaixo for de outra regiao
    | otherwise = filter (> valor_celula) [1..tamanhoRegiao reg (x,y)+1] --devolve os valores maiores que o valor da celula acima para serem filtradas depois
  where
    id_regiao = reg!!x!!y
    valor_celula = tab!!(x+1)!!y

-- Função auxiliar para filtrar valores menores que o da célula na coordenada acima de (x,y) da mesma região
restricaoVerticalBaixo:: Tabuleiro -> Regiao -> (Int, Int) -> [Int]
restricaoVerticalBaixo tab reg (x,y)
     | x == 0 = [] --evita index out of bounds. Itens da primeira linha não tem que se preocupar com ser menor que a célula acima
    | reg!!(x-1)!!y /= id_regiao = [] -- se a celula acima não for da mesma regiao, retorna lista vazia
    | otherwise = filter (<valor_celula)[1..tamanhoRegiao reg (x,y)+1]
   where
    id_regiao = reg!!x!!y
    valor_celula=tab!!(x-1)!!y

-- O filtro será a
--Intersecção da lista de possibilidades atual com o retorno das restricoes verticais
filtraRestricoesVerticais:: Tabuleiro->Regiao->(Int,Int)->[Int]->[Int]
filtraRestricoesVerticais tab reg (x,y) prefiltro = do
    let vertCima = restricaoVerticalCima tab reg (x,y)
    let vertBaixo = restricaoVerticalBaixo tab reg (x,y)
    if vertCima == [] && vertBaixo == []
        then prefiltro
        else
            if vertCima /= [] && vertBaixo == []
                then prefiltro `intersect` vertCima
                else
                    if vertCima == [] && vertBaixo /= []
                        then prefiltro `intersect` vertBaixo
                        else
                            (prefiltro `intersect` vertCima ) `intersect` vertBaixo

-- Retorna uma lista de tuplas com coordenadas e possibilidades de cada célula nula
valoresPossiveisDaRegiao::Tabuleiro->Regiao->Int->[((Int,Int),[Int])]
valoresPossiveisDaRegiao tab reg reg_id= do
  [((i,j),l) | i <-[0.. length tab-1], j <- [0.. length tab-1],ehVazio tab i j && (reg!!i!!j) == reg_id, l <- [restricaoPossibilidades tab reg (i,j)]] 

-- Verifica se algum valor aparece somente na lista de possibilidades de uma célula, se sim, é inserido na região
encontrarUnicaPossibilidade::Tabuleiro->[((Int,Int),[Int])]->Tabuleiro
encontrarUnicaPossibilidade tab [] = tab
encontrarUnicaPossibilidade tab possibilidades = do 
  let valor_unico = valUnicos $ concatVals possibilidades
  let coordendadas_valor_unico = encontrarCoord possibilidades $ head valor_unico --pode ser que exista mais de um valor unico presentes nessas listas por região. Cada iteração trata do primeiro caso apenas
  if valor_unico == []
    then tab
    else atualizaTabuleiro tab (fst (coordendadas_valor_unico), snd (coordendadas_valor_unico), head valor_unico)

concatVals :: [((Int,Int), [Int])] -> [Int] -- Retorna a lista de coords,possibilidades em uma lista apenas de possibilidades
concatVals xs = concatMap snd xs

-- Lista de numeros unicos:
valUnicos :: [Int] -> [Int]
valUnicos xs = filter (\x -> length (filter (== x) xs) == 1) xs

-- Encontrar coordenada onde o numero fornecido é uma das possibilidades
encontrarCoord :: [((Int,Int), [Int])] -> Int -> (Int, Int)
encontrarCoord coords num =
  case filter (elem num . snd) coords of
    [(coord, _)] -> coord

-- Repetir encontrarUnicaPossibilidade:
iteraProcurandoPossibilidade::Tabuleiro->Regiao->Int->Tabuleiro
iteraProcurandoPossibilidade tab _ 0 = tab
iteraProcurandoPossibilidade tab reg n =
  iteraProcurandoPossibilidade (encontrarUnicaPossibilidade tab (valoresPossiveisDaRegiao tab reg n)) reg (n-1)


-- Verificar coordenadas do tabuleiro de valor nulo
coordenadasZero :: Tabuleiro -> [(Int, Int)]
coordenadasZero matrix = [(row, col) | (row, rowVals) <- zip [0..] matrix,
                                     (col, val) <- zip [0..] rowVals,
                                     val == 0]

-- Verifica se a coordenada está na linha mais baixa da região e é a única da sua região na linha
menorSolitarioReg::Regiao -> (Int,Int) -> Bool
menorSolitarioReg reg (x,y) = do
    let regiao_id = (reg!!x)!!y
    let regiao_id_abaixo = if x == length reg -1 then -1 else (reg!!(x+1)!!y)
    length (filter (==regiao_id) (reg!!x)) == 1 && regiao_id /= regiao_id_abaixo

-- Verifica se a coordenada está na linha mais alta da região e é a única da sua região na linha
maiorSolitarioReg :: Regiao -> (Int, Int) -> Bool
maiorSolitarioReg reg (x, y) = do
    let regiao_id = (reg!!x)!!y
    let regiao_id_acima = if x == 0 then -1 else (reg!!(x-1)!!y)
    length (filter (==regiao_id) (reg!!x)) == 1 && regiao_id /= regiao_id_acima

-- Se a coordenada está na posição mais alta/baixa da região, remove o menor/maior item da lista de possibilidades
filtraSolitarioRegiao::Tabuleiro->Regiao->(Int,Int)->[Int]->[Int]
filtraSolitarioRegiao tab reg (x,y) prefiltro
    |menorSolitarioReg reg (x,y) && length prefiltro /= 1 = prefiltro \\ [maximum prefiltro]
     |maiorSolitarioReg reg (x,y) && length prefiltro /= 1 = prefiltro \\ [minimum prefiltro]
    |otherwise = prefiltro

-- Inserir valores no tabuleiro
atualizaTabuleiro :: Tabuleiro -> (Int, Int, Int) -> Tabuleiro
atualizaTabuleiro board (row, col, val) = 
  let (xs, ys) = splitAt row board
      row' = atualizaLinha (head ys) col val
  in xs ++ [row'] ++ tail ys

-- Inserir valor na linha
atualizaLinha :: [Int] -> Int -> Int -> [Int]
atualizaLinha row col val =
  let (xs, ys) = splitAt col row
  in xs ++ [val] ++ tail ys

-- Verificar se o valor da coordenada é 0 (Usado para montar uma lista por isso o Int->Int ao invés de (Int,Int))
ehVazio :: Tabuleiro -> Int -> Int -> Bool
ehVazio board row col = board !! row !! col == 0
 
-- Recursivamente ""chuta"" valores possíveis, caso seja um valor que não causa que outra celula tenha possibilidades = [], chama o solucionador de novo. 
-- Bom para casos onde uma pequena alteração pode implicar em diversas descobertas para o pre-solucionador
testaPossibilidades::Tabuleiro->Regiao->(Int,Int)->[Int]->Tabuleiro
testaPossibilidades tab _ _ [] = tab --Quando a lista de coordenadas com valor 0 esta vazia, retorna o tabuleiro atual
testaPossibilidades tab reg (x,y) (possibilidade:resto) = do
  let novoTab = atualizaTabuleiro tab (x,y,possibilidade) --novoTab = tabuleiro atualizado com o chute atual
  let novaCoordZero = coordenadasZero novoTab -- nova lista de coordenadas com zero
  let num_impossibilidades = length (filter (==[]) $ possibilidadesVazios novoTab reg novaCoordZero []) --numero de celulas com valor 0 que tem possibilidades = [] (naõ tem possibilidades)
  if num_impossibilidades  > 0 --Se o chute atual causou alguma impossibilidade:
    then testaPossibilidades tab reg (x,y) resto -- Ignora o chute atual e testa o próximo
    else --Se não, chama o solucionador de novo para iterar nas coordenadas == 0
      solucionador novaCoordZero novoTab reg--testaPossibilidades tab reg (head novaCoordZero) (restricaoPossibilidades tab reg $ head novaCoordZero) 

-- A partir das coordenadas (com valor ==0), retorna uma lista de possibilidades de cada célula
possibilidadesVazios::Tabuleiro->Regiao->[(Int,Int)]->[[Int]]->[[Int]]
possibilidadesVazios _ _ [] _ = []
possibilidadesVazios tab reg (coord:resto) lista_r= do
   let possibilidades_atual = restricaoPossibilidades tab reg coord
   lista_r ++ [possibilidades_atual] ++ possibilidadesVazios tab reg resto []

-- preSolucionador ≃ solucionador analítico
-- Filtra restricoes, procura possibilidades unicas e atualiza o tabuleiro
preSolucionador :: [(Int,Int)] -> Tabuleiro -> Regiao -> Tabuleiro
preSolucionador [] tab _ = tab
preSolucionador ((linha,coluna):resto) tab reg = do
    let novoTab = iteraProcurandoPossibilidade tab reg (maximum $ concat tab)
    if novoTab!!linha!!coluna == 0
        then let valores_possiveis = restricaoPossibilidades novoTab reg (linha,coluna) \\ [novoTab!!linha!!coluna]
            in if length valores_possiveis == 1 
            then preSolucionador resto (atualizaTabuleiro novoTab (linha,coluna,head valores_possiveis)) reg
            else preSolucionador resto novoTab reg
        else preSolucionador resto novoTab reg
    
-- "loop" principal de solucionar o tabuleiro. Se o preSolucionador retornar o mesmo tabuleiro que foi dado, 
-- significa que atingiu o limite da solução analítica.
-- Neste caso, chamamos o testaPossibilidades para "chutar" um valor de possibilidade na primeira célula == 0 
-- e dentro de testaPossibilidades o solucionador é chamado de novo. 
solucionador :: [(Int, Int)] -> Tabuleiro -> Regiao -> Tabuleiro
solucionador coords tab reg = do
    let novoTab = preSolucionador coords tab reg
        novaCoordZero = coordenadasZero novoTab
    if novoTab == tab 
      then if length novaCoordZero /= 0 
            then testaPossibilidades novoTab reg (head $ coordenadasZero novoTab) $ restricaoPossibilidades novoTab reg (head $ coordenadasZero novoTab) 
            else  novoTab
      else solucionador coords novoTab reg

--Retorna o tamanho da regiao de dada coordenada
tamanhoRegiao :: Regiao -> (Int,Int) ->  Int
tamanhoRegiao reg (x,y) = length $ filter (==(reg!!x!!y)) $ concat reg

printTabuleiro ::Tabuleiro -> IO ()
printTabuleiro = mapM_ print


main :: IO ()
main = do
  let tab = tabuleiro13
  let reg = regioes13
  let coords = coordenadasZero tab
  let r = solucionador coords tab reg
  if length (coordenadasZero r) > 0
    then print "Sem solucao"
    else printTabuleiro r

  
