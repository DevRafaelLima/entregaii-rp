{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}


module Main where

import LerArq ( file, lista, timesL, abrirArq, criaTimes ) 
import Calculos ( podio, sortGT, totalVitorias, totalEmpates, totalDerrotas, listaCampeonato, listaTimes, aproveitamento, totalPontos, totalGols, totalGolsSofridos )
import Impressoes ( classifGeral, classifTime, retornaClassificacao, retornaRodada, retornaPodio, retornaRebaixados )
import System.IO ( IOMode(ReadMode), hClose, openFile )
import System.IO.Error
import Control.Exception
import Numeric


formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

-- 1  OK
-- 2  OK
-- 3  OK
-- 4  OK
-- 5  OK
-- 6  OK
-- 7  OK
-- 8  OK
-- 9  OK

-- exemplos de entrada:

-- buscaPorTime "Grêmio"
-- buscaPorRodada "1" "Grêmio"

-- esses não precisam de argumento:

-- classificacao
-- top3
-- rebaixados


buscaPorTime :: String -> IO String
buscaPorTime time = do
             ls <- Calculos.listaCampeonato                     -- 1 Qual o número de vitórias, empates e derrotas
             tv <- Calculos.totalVitorias time ls
             te <- Calculos.totalEmpates time ls
             td <- Calculos.totalDerrotas time ls

             lsTime <- Calculos.listaTimes                      -- 2 Qual a classficação do time
             aproveitamento <- Calculos.aproveitamento time     -- 3 aproveitamento do time
             pts <- Calculos.totalPontos time                   -- 6 pontos do time
             gols <- Calculos.totalGols time
             golsSofridos <- Calculos.totalGolsSofridos time    -- 4 saldo de gols (gols-golsSofridos)
                                   

             return ( "\n" ++ "\n" ++
                    "====== Desempenho no Campeonato =====" ++ "\n" ++
                    "\n" ++ "O "++ classifTime time lsTime ++ "\n" ++ "\n" ++
                    "Vitória(s): " ++ show tv ++ "\n" ++
                    "Empate(s) : "++ show te ++ "\n" ++
                    "Derrota(s): " ++ show td ++ "\n" ++ "\n" ++

                    "Aproveitamento : " ++ formatFloatN aproveitamento 2 ++ "\n" ++
                    "Total de Gols  : " ++ show gols ++ "\n" ++
                    "Total de Pontos: " ++ show pts  ++ "\n" ++
                    "Saldo de Gols  : " ++ show (gols-golsSofridos)  ++ "\n" ++ "\n" ++
                    "====================================="  ++ "\n" ++ "\n")


buscaPorRodada:: String -> String -> IO String-- 5 resultado partida X time X
buscaPorRodada rodada time  = do
                                  resultado <- Impressoes.retornaRodada rodada time
                                  return( "\n" ++ "\n" ++ 
                                          "=============== Resultado da Partida ==============" ++ "\n" ++
                                          "\n" ++ resultado ++ "\n" ++ "\n" ++
                                          "===================================================" ++ "\n" ++ "\n") 



classificacao :: IO String-- 9 classificação geral
classificacao = do 
                   clsfG <- Impressoes.retornaClassificacao
                   return ( "\n" ++ "\n" ++
                           "====== Classificação Geral =====" ++ "\n" ++
                           "\n" ++ clsfG ++ "\n" ++
                           "================================")


top3 :: IO String-- 7 primeiras 3 colacações 
top3 = do 
       pod <- Impressoes.retornaPodio 
       return ( "\n" ++ "\n" ++
              "=============== Pódio ================" ++ "\n" ++
              "\n" ++ pod ++ "\n" ++
              "======================================")


rebaixados :: IO String-- 9 rebaixados
rebaixados = do 
                reb <- Impressoes.retornaRebaixados
                return ( "\n" ++ "\n" ++
                         "=============== Times Rebaixados ================" ++ "\n" ++
                         "\n" ++ reb ++ "\n" ++
                         "=================================================")

endProg :: IO String
endProg = return ("=============Obrigado===============")

-- main:: IO String
main = do
  putStrLn ("Bem vindo ao campeonato")
  putStrLn ("[1] - Mostrar Classificação geral")
  putStrLn ("[2] - Consultar Times rebaixados")
  putStrLn ("[3] - Consultar o Podio de times")
  putStrLn ("[4] - Consultar Desempenho de um Time")
  putStrLn ("[5] - Resultado de uma partida")
  putStrLn ("Digite qualquer outra coisa e presinone enter para sair!")
  option <- getLine
  case option of
    "1" -> classificacao
    "2"-> rebaixados
    "3" -> top3
    "4" -> do
      putStrLn "Digite o nome do time: "
      c <- getLine
      buscaPorTime c
    "5" -> do
      putStrLn "Digite o número da rodada [Somente números naturais de 1 a 18]: "
      c <- getLine
      putStrLn "Digite o nome do time: "
      d <- getLine
      buscaPorRodada c d
    _ -> endProg
      