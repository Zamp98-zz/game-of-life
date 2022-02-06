import System.IO
imprimeTabela ::   [String] -> Int->Int-> Int -> IO()
imprimeTabela tabela linha coluna 0 = putStr "\n"
imprimeTabela tabela linha coluna i
   | i > 0 = do 
      let l = take coluna tabela
      --print(l)
      imprime l
      let k = drop coluna tabela
      imprimeTabela k linha coluna (i - 1)

imprime :: [String] -> IO()
imprime [] = putStr "\n"

imprime l
   |  length l > 0 = do
      let h = head l
      let x = drop 1 l 
      putStr (h ++ " ")
      imprime x
   | otherwise = imprime []

--- leitura da entrada do jogo. Pegando o número de iterações, linhas, colunas e a própria tabela do jogo vindas de um arquivo txt.

lerArquivo :: FilePath -> IO (Int , Int, Int, [String])
lerArquivo file = fmap (transformaLinhas . words) (readFile file)
-- Guard que conta quantos são vivos, mortos e zumbis
count n linhas colunas [] (a,b,c) = (a,b,c)
count n linhas colunas indexes (a,b,c)
    | i < 0 || j < 0 = count n linhas colunas body (a, b, c)
    | i >= linhas || j >= colunas = count n linhas colunas body (a, b, c)
    | n!!index == "m" = count n linhas colunas body (a, b + 1, c)
    | n!!index == "v" = count n linhas colunas body (a + 1, b, c)
    | n!!index == "z" = count n linhas colunas body (a, b, c + 1)
    where body = tail indexes
          i = head indexes !! 0
          j = head indexes !! 1
          index = i * colunas + j
-- lista todos os adjacentes a uma coordenada i j
adjacentes i j = [[i-1,j+1], [i,j+1], [i+1,j+1],[i-1,j], [i+1,j],[i-1,j-1],[i, j-1], [i+1,j-1]] 
totalAdjacentes n linhas colunas i = count n linhas colunas (adjacentes (div i colunas) (mod i colunas)) (0,0,0)
readInt :: String -> Int
readInt = read
first (vivo,_,_) = vivo
second (_,morto,_) = morto
third (_,_,zumbi) = zumbi
--aplica as regras de vivo, morto ou zumbi, de acordo com suas adjacencias
aplicarRegras n linhas colunas i 
   | n!!i == "m" && vivo == 3 = "v"
   | n!!i == "v" && zumbi >= 1 = "z"
   | n!!i == "v" && vivo < 2  = "m"
   | n!!i == "v" && vivo > 3  = "m"
   | n!!i == "z" && vivo == 0 = "m"
   | otherwise = n!!i
   where vivo = first (totalAdjacentes n linhas colunas i)
         morto = second (totalAdjacentes n linhas colunas i)
         zumbi = third (totalAdjacentes n linhas colunas i)

--transforma as linhas lidas em iterações, linhas, colunas e a tabela do jogo
transformaLinhas :: [String] -> (Int , Int, Int, [String])
transformaLinhas (iteracoes : linhas : colunas : tabela ) = 
    (n, l, c, t)
    where 
        n = readInt iteracoes
        l = readInt linhas
        c = readInt colunas
        t = tabela
--percorre a tabela do jogo aplicando as regras
percorre n linhas colunas b i 
   | i >= linhas * colunas = b
   | otherwise = percorre n linhas colunas c (i + 1)
   where c = (aplicarRegras n linhas colunas i):b
life n linhas colunas i total
  | i > 0 && m /= n = do 
      --print ("Iteracao ", i)
      putStr "Iteracoes restantes "
      print i
      putStr "\n"
      imprimeTabela n linhas colunas linhas
      life m linhas colunas (i - 1) total
  | i > 0 && m == n = do 
      putStr "Fim de Jogo\n"
      print (total - i)
  | otherwise = do 
      putStr "Resultado\n"
      imprimeTabela n linhas colunas linhas
  where m = reverse (percorre n linhas colunas [] 0)
startLife n tabela linhas colunas = do
   life tabela linhas colunas n n

main = do
   
   (n1, linhas1, colunas1, tabela1) <- lerArquivo "entrada1.txt"
   putStr "\nentrada 1:\n"
   imprimeTabela tabela1 linhas1 colunas1 linhas1
   startLife n1 tabela1 linhas1 colunas1  

   (n2, linhas2, colunas2, tabela2) <- lerArquivo "entrada2.txt"
   putStr "\nentrada 2:\n"
   imprimeTabela tabela2 linhas2 colunas2 linhas2
   startLife n2 tabela2 linhas2 colunas2 
   
   (n3, linhas3, colunas3, tabela3) <- lerArquivo "entrada3.txt"
   putStr "\nentrada 3:\n"
   imprimeTabela tabela3 linhas3 colunas3 linhas3
   startLife n3 tabela3 linhas3 colunas3 

   (n4, linhas4, colunas4, tabela4) <- lerArquivo "entrada4.txt"
   putStr "\nentrada 4:\n"
   imprimeTabela tabela4 linhas4 colunas4 linhas4
   startLife n4 tabela4 linhas4 colunas4 
   

   (n5, linhas5, colunas5, tabela5) <- lerArquivo "entrada5.txt"
   putStr "\nentrada 5:\n"
   imprimeTabela tabela5 linhas5 colunas5 linhas5
   startLife n5 tabela5 linhas5 colunas5 

   (n6, linhas6, colunas6, tabela6) <- lerArquivo "entrada6.txt"
   putStr "\nentrada 6:\n"
   imprimeTabela tabela6 linhas6 colunas6 linhas6
   startLife n6 tabela6 linhas6 colunas6 