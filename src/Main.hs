module Main where

import Data.Maybe ( fromMaybe )
import GameOfLife
import Graphics.Gloss
import Grids (emptyGrid)
import Immigration (playIGameOfLife)

main :: IO ()
main = do
    inputGrid <- readFile "src/input.txt"
    let l = columns * length (lines inputGrid)
    let startGrid = if l == gridSize then lines inputGrid 
                    else emptyGrid

    let defaultRules = [(1, 3), (1, 2), (0, 3)]

    controls <- readFile "src/controls.txt"
    putStrLn $ unlines $ lines controls

    putStrLn "|--Menu--------Game of Life--------------|"
    putStrLn "|________________________________________|"
    putStrLn "| Opção 1 - Regras Padrão.               |"
    putStrLn "| Opção 2 - Regras Customizadas.         |"
    putStrLn "| Opção 3 - The Immigration Game.        |"
    putStrLn "|________________________________________|"
    putStrLn "Escolha uma opção: "
    c <- getLine
    case c of
        "2" -> do
            printDefaultRules
            putStrLn ""
            putStrLn "|-> O valor para a Regra 2 deve ser menor que o valor para a Regra 1. |"
            putStrLn "|-> O valor para todas as Regras devem estar entre 1 e 8.             |"
            putStrLn "|-> Caso contrário, as regras padrão serão carregadas.                |"
            putStrLn ""
            putStrLn "Valor para a Regra 1 - Morte, por superpopulação se número de vizinhos maior que:"
            overpop  <- getLine
            putStrLn "Valor para a Regra 2 - Morte, por solidão se número de vizinhos é menor que:"
            underpop <- getLine
            putStrLn "Valor para a Regra 3 - Nascimento, por reprodução se número de vizinhos é igual a:"
            reprod   <- getLine
            
            let rules = fromMaybe defaultRules $ validRules (read overpop :: Int) (read underpop :: Int) (read reprod :: Int)

            play window background fps (initialGrid startGrid rules) drawGrid handleKeys update
                where window = InWindow "Game of Life - Custom" (screenSizeX, screenSizeY) (50, 50)         

        "3" -> do 
            printImmigrationRules
            playIGameOfLife

        _   -> do
            printDefaultRules
            let rules = defaultRules
            play window background fps (initialGrid startGrid rules) drawGrid handleKeys update
                where window = InWindow "Game of Life" (screenSizeX, screenSizeY) (50, 50)

validRules :: Int -> Int -> Int -> Maybe [(Int, Int)]
validRules overpop underpop reprod
    | cond0 && cond1 && cond2 = Just rules
    | otherwise               = Nothing
    
    where
        cond0 = overpop > underpop
        cond1 = overpop > 0 && underpop > 0 && reprod > 0
        cond2 = overpop < 9 && underpop < 9 && reprod < 9
        rules = [(1,y) | y <- [underpop..overpop]] ++ [(0,reprod)]

background :: Color
background = white

fps :: Int
fps = 3

printRules :: IO()
printRules = do
    putStrLn "|________________________________________________________________|"
    putStrLn "| Regra 1 -> Morte por superpopulação  se número de vizinhos >  3|"
    putStrLn "| Regra 2 -> Morte por solidão         se número de vizinhos <  2|"
    putStrLn "| Regra 3 -> Nascimento por reprodução se número de vizinhos == 3|"

printDefaultRules :: IO()
printDefaultRules = do
    putStrLn "|--Regras-----------Valores padrão-------------------------------|"
    printRules
    putStrLn "|________________________________________________________________|"   

printImmigrationRules :: IO()
printImmigrationRules = do
    putStrLn "|--Regras-----------The Immigration Game-------------------------|"
    printRules
    putStrLn "| Célula que nasce tem a cor igual a da maioria de seus vizinhos |"
    putStrLn "|________________________________________________________________|"       


