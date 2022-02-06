import System.IO
import Control.Monad





imprime ::   [String] -> Int->Int-> Int -> IO()
imprime tabela linha coluna 0 = putStr "\n"
imprime tabela linha coluna i
   | i > 0 = do 
      let l = take coluna tabela
      --print(l)
      imprime2 l
      let k = drop coluna tabela
      imprime k linha coluna (i - 1)

imprime2 :: [String] -> IO()
imprime2 [] = putStr "\n"

imprime2 l
   |  length l > 0 = do
      let h = head l
      let x = drop 1 l 
      putStr (h ++ " ")
      imprime2 x
   | otherwise = imprime2 []
   


-- n é a quantidade de vezes que vai rodar




--- leitura da entrada do jogo. Pegando o número de iterações, linhas, colunas e a própria tabela do jogo.

readMatriz :: FilePath -> IO (Int , Int, Int, [String])
readMatriz file = fmap (parseLinhas . words) (readFile file)


readInt :: String -> Int
readInt = read


parseLinhas :: [String] -> (Int , Int, Int, [String])
parseLinhas (iteracoes : linhas : colunas : matriz ) = 
    (x, l, c, m)
    where 
        x = readInt iteracoes
        l = readInt linhas
        c = readInt colunas
        m = matriz

main = do
   
   (n1, linhas1, colunas1, tabela1) <- readMatriz "entrada1.txt"
   putStr "\nentrada 1:\n"
   imprime tabela1 linhas1 colunas1 linhas1

   (n2, linhas2, colunas2, tabela2) <- readMatriz "entrada2.txt"
   putStr "\nentrada 2:\n"
   imprime tabela2 linhas2 colunas2 linhas2
   
   (n3, linhas3, colunas3, tabela3) <- readMatriz "entrada3.txt"
   putStr "\nentrada 3:\n"
   imprime tabela3 linhas3 colunas3 linhas3


   (n4, linhas4, colunas4, tabela4) <- readMatriz "entrada4.txt"
   putStr "\nentrada 4:\n"
   imprime tabela4 linhas4 colunas4 linhas4
   

   (n5, linhas5, colunas5, tabela5) <- readMatriz "entrada5.txt"
   putStr "\nentrada 5:\n"
   imprime tabela5 linhas5 colunas5 linhas5