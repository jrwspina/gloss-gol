module GameOfLife where

import Data.Maybe ( fromMaybe )
import Graphics.Gloss
    ( color,
      pictures,
      rectangleSolid,
      rectangleWire,
      translate,
      makeColorI,
      Picture )
import Graphics.Gloss.Interface.Pure.Game
    ( color,
      pictures,
      rectangleSolid,
      rectangleWire,
      translate,
      makeColorI,
      Picture,
      Key(MouseButton, Char, SpecialKey),
      KeyState(Down, Up),
      MouseButton(LeftButton),
      SpecialKey(KeySpace),
      Event(EventKey) )
import Grids (emptyGrid, stillLives, gosperGliderGun, oscillators, spaceships)

{- Type def. -}

type Coordinate = (Int, Int)
type Cell = (Coordinate, Int)
type Paused = Bool
type Rules = [(Int, Int)]
type Grid = ([Cell], Paused, Rules)

{- Size Variables -}

screenSizeX :: Int
screenSizeX = 1480

screenSizeY :: Int
screenSizeY = 960

rows :: Int
rows = 30

columns :: Int
columns = 60

gridSize :: Int
gridSize = rows * columns

cellDimension :: Float
cellDimension = 30

{- Grid Parsing -}

statMap :: Char -> Int
statMap c
    | c == 'x'  = 1
    | otherwise = 0

parsePatternGrid :: [String] -> [Cell]
parsePatternGrid s = zip coord grid
    where
        coord = [(x, y) | y<-[0..rows-1], x<-[0..columns-1]]
        grid  = map statMap $ filter (/= '\n') $ unlines $ reverse s

initialGrid :: [String] -> Rules -> Grid
initialGrid s rules = (parsePatternGrid s, True, rules)

{- Logic -}

iterateGrid :: Grid -> Grid
iterateGrid (grid, False, rules) = (map (iterateCell grid rules) grid, False, rules)
iterateGrid (grid, _    , rules) = (grid, True, rules)

getNeighbours :: Coordinate -> [Coordinate]
getNeighbours (x, y) = [(x + x', y + y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1], (x', y') /= (0,0)]

sumNeighboursStatus :: [Cell] -> Coordinate -> Int
sumNeighboursStatus grid coord = sum [fromMaybe 0 $ lookup n grid | n <- getNeighbours coord]

iterateCell :: [Cell] -> Rules -> Cell -> Cell
iterateCell grid rules ((x, y), status)
    | (status, nStatus) `elem` rules = ((x, y), 1)
    | otherwise                      = ((x, y), 0)

    where
        nStatus = sumNeighboursStatus grid (x, y)

update :: Float -> Grid -> Grid
update _ = iterateGrid

{- Input Handling -}

switchCellState :: Coordinate -> Cell -> Cell
switchCellState (x', y') ((x, y), z)
    | (x', y') == (x, y) && z == 0 = ((x, y), 1)
    | (x', y') == (x, y)           = ((x, y), 0)
    | otherwise                    = ((x, y), z)

manualGrid :: [Cell] -> Coordinate -> [Cell]
manualGrid grid coord = map (switchCellState coord) grid

convertCoord :: (Float, Float) -> Coordinate
convertCoord (x', y') = ( round ((x' + (fromIntegral screenSizeX * 0.6))  / cellDimension)
                        , round ((y' + (fromIntegral screenSizeY * 0.5 )) / cellDimension)
                        )

handleKeys :: Event -> Grid -> Grid

-- Patterns/Reset 
handleKeys (EventKey (Char 'r') _ _ _) (_, _, rules) = initialGrid emptyGrid       rules
handleKeys (EventKey (Char '1') _ _ _) (_, _, rules) = initialGrid stillLives      rules
handleKeys (EventKey (Char '2') _ _ _) (_, _, rules) = initialGrid oscillators     rules
handleKeys (EventKey (Char '3') _ _ _) (_, _, rules) = initialGrid spaceships      rules
handleKeys (EventKey (Char '4') _ _ _) (_, _, rules) = initialGrid gosperGliderGun rules

-- Pause/Play
handleKeys (EventKey (SpecialKey KeySpace) Up _ _) (grid, True,  rules) = (grid, False, rules)
handleKeys (EventKey (SpecialKey KeySpace) Up _ _) (grid, False, rules) = (grid, True , rules)

-- Mouse
handleKeys (EventKey (MouseButton LeftButton) Down _ (x, y)) (grid, True, rules) = (manualGrid grid coord, True, rules)
    where 
        coord = convertCoord (x, y)

--Other        
handleKeys _ grid = grid

{- Rendering -}

drawCell :: Int -> Picture
drawCell 1  = color (makeColorI 0 206 209 255) $ rectangleSolid cellDimension cellDimension
drawCell _  =                                    rectangleWire  cellDimension cellDimension

drawGrid :: Grid -> Picture
drawGrid (grid, _, _) = translate (fromIntegral screenSizeX * (-0.6))
                                  (fromIntegral screenSizeY * (-0.5))
                                  frame            
        where frame = pictures [
                                translate (fromIntegral x *cellDimension) 
                                          (fromIntegral y *cellDimension) 
                                          (drawCell z) 
                                          | ((x, y), z) <- grid
                               ]
 