{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module LerArq(file, lista, timesL, abrirArq, criaTimes, Campeonato(rodada, t1, t2, gt1, gt2)) where

import System.IO
import Data.Graph as String
import Data.List.Split (splitOn)

-- Estrutura de dados para campeonato
data Campeonato = Campeonato {
    rodada :: String,
    t1    :: String,
    t2    :: String,
    gt1   :: Int,
    gt2   :: Int
} deriving Show

file :: String
file = "C:\\Users\\StartSe\\Documents\\grupo-07\\Entrega 2\\campeonato.txt"

-- lista de dados de um campeonato
lista:: [Campeonato]
lista = []

timesL:: [String]
timesL = []


-- fun��o para adicionar a lista informada
-- args   : item e lista
-- return : lista 
insertt :: a -> [a] -> [a]
insertt x [] = [x]
insertt x (y:ys) = x:y:ys


-- função responsável por ler o arquivo txt dividir os dados
-- e por em uma lista de "campeonatos" o lista estão na main
-- na variável, foi utilizada a técnica da gambiarra para
-- se ter o retorno.
abrirArq :: Handle -> [Campeonato] -> IO [Campeonato]
abrirArq file lst =
    do
        isFileEnd <- hIsEOF file
        if isFileEnd
            then
                return lst
            else do
                    info <- hGetLine file
                    let str = splitOn ";" info
                    let c = Campeonato (head str) (str !! 1) (str !! 2)
                                       (read(str !! 3)::Int) (read(str !! 4)::Int)
                    abrirArq file (insertt c lst)


-- Cria uma lista com o nome de todos times sem duplicatas para
-- eu pode usar na minha gambiarra de fazer a classifica��o
-- o importante � q no meu pc funciona
-- SoFtWaRe eNgInEeR 
criaTimes :: Handle -> [String] -> IO [String]
criaTimes file lst =
    do
        isFileEnd <- hIsEOF file
        if isFileEnd
            then
                return lst
            else do
                    info <- hGetLine file
                    let str = splitOn ";" info
                    if not (existeVer (str !! 1) lst) then
                        criaTimes file (insertt (str !! 1) lst)
                    else
                        if not (existeVer (str !! 2) lst) then
                            criaTimes file (insertt (str !! 2) lst)
                        else
                            criaTimes file lst


-- verifica se o dado j� foi adicionado a lista passada de par�metro
-- arg : "eu existo?" lista
-- rtrn: Sim/N�o
existeVer::String -> [String] -> Bool
existeVer s [] = False
existeVer s (x:xs) = if s == x then True else existeVer s xs

