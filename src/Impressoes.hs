module Impressoes where

import LerArq
import Calculos



-- função só para diminuir a quantidade de parametros informados
-- basta informar o time e a lista para fazer a busca da classficação
-- de X time
classifTime :: String -> [(Int,String)] -> String
classifTime s (x:xs) = buscaPosPodio 1 s (x:xs)

-- busca o time informado e incrementa o integer para 
-- representar a posição ou indice
-- a lista informada tem que vir de forma ordenada!
buscaPosPodio:: Int -> String -> [(Int, String)]-> String
buscaPosPodio i s [] = "Lista vazia!"
buscaPosPodio i s (y:ys) = if comparaTupla s y
                                then s ++ " esta na " ++ show i ++ show ['º'] ++ " posicao"
                           else
                              buscaPosPodio (i+1) s ys

-- função auxiliar de comparação de string e segundo item de uma tupla
comparaTupla :: String -> (Int,String) -> Bool
comparaTupla s s1 = s == getTime s1

-- retorna o segundo item da tupla para ser comparada na função comparaTupla
getTime :: (Int,String) -> String
getTime (_, t)= t


-- cria uma string com a classificação geral do campeonato
classifGeral :: [(Int, String)] -> String
classifGeral (x:xs) = mostraClass (x:xs) "" 1

-- concatena as strings com a posição
mostraClass :: [(Int, String)] -> String -> Int -> String
mostraClass [y] s i    = s
mostraClass (x:xs) s i = mostraClass xs (s ++ (show i ++ ['º'] ++ " " ++ getTime x ++ " " ++ "\n")) (i+1)


-- faz a chamada do buscaRodada passando como parametro os mesmos parametros
resRodada :: [Campeonato] -> String -> String -> String
resRodada [] _ _ = "Informe todos os dados"
resRodada (x:xs) r t = buscaRodada (x:xs) r t

-- faz a busca da rodada informando
-- a lista a rodada ex.: "1" e o nome do time ex.: "Gremio"
buscaRodada  :: [Campeonato] -> String -> String -> String
buscaRodada (x:xs) r t
  | r < "1" = "Verifique se informou corretamente o número da rodada" 
  | r > "18" = "Verifique se informou corretamente o número da rodada"
  | r == rodada x && t ==  t1 x =
       if gt1 x > gt2 x then
           "O " ++ t1 x ++ " ganhou de " ++ show (gt1 x) ++ " a " ++ show (gt2 x) ++ " contra o " ++ t2 x
       else if gt1 x == gt2 x then
           "O " ++ t1 x ++ " empatou com " ++ show (gt1 x) ++ " a " ++ show (gt2 x) ++ " contra o " ++ t2 x
       else
          "O " ++ t1 x ++ " perdeu de " ++ show (gt2 x) ++ " a " ++ show (gt1 x) ++ " contra o " ++ t2 x
  | r == rodada x && t == t2 x =
   if gt2 x > gt1 x then
        "O " ++ t2 x ++ " ganhou de " ++ show (gt2 x) ++ " a " ++ show (gt1 x) ++ " contra o " ++ t1 x
   else if gt2 x == gt1 x then
           "O " ++ t2 x ++ " empatou com " ++ show (gt2 x) ++ " a " ++ show (gt1 x) ++ " contra o " ++ t1 x     
   else
       "O " ++ t2 x ++ " perdeu de " ++ show (gt1 x) ++ " a " ++ show (gt2 x) ++ " contra o " ++ t1 x
  | otherwise =
    buscaRodada xs r t



retornaClassificacao:: IO String
retornaClassificacao = do
                         classifGeral <$> Calculos.listaTimes


retornaRodada :: String -> String -> IO String
retornaRodada rodada time = do
                              lista <- Calculos.listaCampeonato
                              return (resRodada lista rodada time)


retornaPodio :: IO String
retornaPodio = do lista <- Calculos.listaTimes 
                  return ("            1º " ++ getTime (lista !! 0) ++ "\n" ++
                          "            2° " ++ getTime (lista !! 1) ++ "\n" ++
                          "            3° " ++ getTime (lista !! 2) ++ "\n")


retornaRebaixados :: IO String
retornaRebaixados = do 
                    lista <- Calculos.listaTimes 
                    x <- return (reverse lista)
                    return ("               " ++ getTime (x !! 1) ++ "\n" ++
                            "               " ++ getTime (x !! 2) ++ "\n" ++
                            "               " ++ getTime (x !! 3) ++ "\n")
                            -- "               " ++ getTime (x !! 4) ++ "\n")