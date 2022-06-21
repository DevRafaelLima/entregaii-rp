{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Calculos ( podio, sortGT, totalVitorias, totalEmpates, totalDerrotas, listaCampeonato, listaTimes, aproveitamento, totalPontos, totalGols, totalGolsSofridos ) where

import System.IO
import Data.List(sortBy)
import LerArq


instance {-# OVERLAPPING #-} Show String where
    show = id



-- recebe como parâmetros um valor inicial inteiro no caso 0 pois ele será o acumulador....
-- uma string equivalente ao nome do time
-- e a lista contendo todos campeonatos

vitorias:: Int -> String -> [Campeonato] -> IO Int
vitorias a s [] = return a
vitorias a s (x:xs) = do
                    if (s == t1 x) && (gt1 x > gt2 x) || (s == t2 x) && (gt2 x > gt1 x) then
                        vitorias (a+1) s xs
                    else
                        vitorias a s xs

derrotas:: Int -> String -> [Campeonato] -> IO Int
derrotas a s [] = return a
derrotas a s (x:xs) = do
                    if (s == t1 x) && (gt1 x < gt2 x) || (s == t2 x) && (gt2 x < gt1 x) then
                        derrotas (a+1) s xs
                    else
                        derrotas a s xs

empates:: Int -> String -> [Campeonato] -> IO Int
empates a s [] = return a
empates a s (x:xs) = do
                    if (s == t1 x) && (gt1 x == gt2 x) || (s == t2 x) && (gt2 x == gt1 x) then
                        empates (a+1) s xs
                    else
                        empates a s xs

-- retorna o total de vitórias, derrotas e empates do time informado
-- lista com os dados de campeonato

totalVitorias :: String -> [Campeonato] -> IO Int
totalVitorias c [] = return 0
totalVitorias c (x:xs) =  vitorias 0 c (x:xs)

totalEmpates :: String -> [Campeonato] -> IO Int
totalEmpates c []     = return 0
totalEmpates c (x:xs) = empates 0 c (x:xs)

totalDerrotas :: String -> [Campeonato] -> IO Int
totalDerrotas c []     = return 0
totalDerrotas c (x:xs) = empates 0 c (x:xs)


-- classifica��o
-- cada partida vencida equivale a 3 pontos
-- cada partida empate  equivale a 1 ponto
-- ganha o time com maior pontua��o
-- em caso de empate os par�metros ser�o levados em conta: nmrs de vit�ria, saldo de gols e gols pr�
-- recebe como parametro a lista do campeonato
-- uma tupla qualquer s� pra iniciar pra poder concatenar se conseguirem tentem arrumar pra sl� nulo
-- e por �ltimo a lista com somente o nome dos times participando do campeonato
podio :: [Campeonato] -> [(Int,String)] -> [String] -> IO [(Int, String)]
podio (z:zs) (y:ys) [] = return (y:ys)
podio (z:zs) (y:ys) (x:xs) = do
                  h <- pontos (z:zs) x
                  podio (z:zs) ((y:ys) ++ [(h, x)]) xs



-- faz o calculo de pontos do time informado
pontos :: [Campeonato] -> String -> IO Int
pontos [] s = return 0
pontos (x:xs) s = do
                     t <- totalVitorias s (x:xs)
                     e <- totalEmpates  s (x:xs)
                     return ((t * 3) + e)








-- isso se for chamado ordena a lista
--print (reverse (sortBy sortGT lista))
sortGT (a1, b1) (a2, b2)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | a1 == a2 = compare b1 b2


fileHandle = openFile file ReadMode

-- apartir daqui foi eu que implementei
listaCampeonato :: IO[Campeonato]
listaCampeonato =  do
                   x <- fileHandle
                   hClose x 
                   fileHandle <- openFile file ReadMode               
                   abrirArq fileHandle lista
    
                   

listaTimes :: IO[(Int,String)]
listaTimes = do
                x <- fileHandle
                hClose x 
                fileHandle<- openFile file ReadMode
                times <- criaTimes fileHandle timesL
                lista <- listaCampeonato
                n <- Calculos.podio lista [(0,"")] times -- lista de tuplas de time e seus pontos no campeonato
                return (sortBy sortGT n) -- lista tupla de times e seus respectivos pontos ordenadas por pontos



totalPontos:: String -> IO Int
totalPontos time = do
                    lista <- listaCampeonato
                    pontos lista time


-- Calcula o aproveitamento de determinado time
-- Arg: String <- Nome do time
-- Return: Float <- Porcentagem do aproveitamento
aproveitamento :: String -> IO Float
aproveitamento a = do
            lista <- listaCampeonato
            c <- pontos lista a
            return (((fromIntegral c) / (fromIntegral 53)) * (fromIntegral 100))



contaGols:: Int -> String -> [Campeonato] -> IO Int
contaGols aux time [] = return aux
contaGols aux time (x:xs) = do
                    if time == t1 x then
                        contaGols (aux + (gt1 x) ) time xs
                    else
                        if time == t2 x then
                            contaGols (aux + (gt2 x) )time xs
                        else 
                            contaGols aux time xs



contaGolsSofridos :: Int -> String -> [Campeonato] -> IO Int
contaGolsSofridos aux time [] = return aux
contaGolsSofridos aux time (x:xs) = do
                    if time == t1 x then
                        contaGols (aux + (gt2 x) ) time xs
                    else
                        if time == t2 x then
                            contaGolsSofridos (aux + (gt1 x) )time xs
                        else 
                            contaGolsSofridos aux time xs

totalGolsSofridos :: String -> IO Int
totalGolsSofridos time = do 
                            lista <- listaCampeonato
                            contaGolsSofridos 0 time lista

totalGols ::String -> IO Int
totalGols time = do
                    lista <- listaCampeonato
                    contaGols 0 time lista