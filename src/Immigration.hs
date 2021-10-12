module Immigration where

import Data.Maybe ( fromMaybe )
import GameOfLife (screenSizeX, screenSizeY, rows, columns, gridSize, cellDimension, getNeighbours, convertCoord, Coordinate, Cell, Paused)
import Graphics.Gloss
    ( Picture,
      white,
      color,
      pictures,
      rectangleSolid,
      rectangleWire,
      translate,
      play,
      makeColorI,
      Display(InWindow),
      Color )
import Graphics.Gloss.Interface.Pure.Game
    ( Event(EventKey),
      Key(MouseButton, Char, SpecialKey),
      KeyState(Down, Up),
      MouseButton(LeftButton),
      SpecialKey(KeySpace) )
import Grids (emptyGrid, gosperGliderGun, oscillators, spaceships, stillLives)

{- Type def. -}

type Grid = ([Cell], Paused)

{- Grid Parsing -}

statMap :: Char -> Int
statMap c
    | c == 'x'  = 1
    | c == 'o'  = 2
    | otherwise = 0

parsePatternGrid :: [String] -> [Cell]
parsePatternGrid s = zip coord grid
    where
        coord = [(x, y) | y<-[0..rows-1], x<-[0..columns-1]]
        grid  = map statMap $ filter (/= '\n') $ unlines $ reverse s

initialGrid :: [String] -> Grid
initialGrid s = (parsePatternGrid s, True)

{- Logic -}

iterateGrid :: Grid -> Grid
iterateGrid (grid, False) = (map (iterateCell grid) grid, False)
iterateGrid (grid, _    ) = (grid, True)

sumNeighboursStatus :: [Cell] -> Coordinate -> Int
sumNeighboursStatus grid coord = sum $ map allOne $ filter (/= 0) [fromMaybe 0 $ lookup n grid | n <- getNeighbours coord]
    where
        allOne _ = 1 -- Transforms 2 into 1

majorityState :: [Cell] -> Coordinate -> Int
majorityState grid coord = if m > 4 then 2 else 1 
    where 
        m = sum [fromMaybe 0 $ lookup n grid | n <- getNeighbours coord]

iterateCell :: [Cell] -> Cell -> Cell
iterateCell grid ((x, y), status)
    | (status, nStatus) `elem` survives = ((x, y), status)
    | (status, nStatus)   ==   (0,3)    = ((x, y), imStatus)
    | otherwise                         = ((x, y), 0)

    where
        survives = [(2, 2), (2, 3), (1, 2), (1, 3)]
        nStatus  = sumNeighboursStatus grid (x, y)
        imStatus = majorityState grid (x, y)

update :: Float -> Grid -> Grid
update _ = iterateGrid

{- Input Handling -}

switchCellState :: Coordinate -> Cell -> Cell
switchCellState (x', y') ((x, y), z)
    | (x', y') == (x, y) && z == 0 = ((x, y), 1)
    | (x', y') == (x, y) && z == 1 = ((x, y), 2)
    | (x', y') == (x, y)           = ((x, y), 0)
    | otherwise                    = ((x, y), z)

manualGrid :: [Cell] -> Coordinate -> [Cell]
manualGrid grid coord = map (switchCellState coord) grid

handleKeys :: Event -> Grid -> Grid

-- Patterns/Reset 
handleKeys (EventKey (Char 'r') _ _ _) (_, _) = initialGrid emptyGrid       
handleKeys (EventKey (Char '1') _ _ _) (_, _) = initialGrid stillLives     
handleKeys (EventKey (Char '2') _ _ _) (_, _) = initialGrid oscillators    
handleKeys (EventKey (Char '3') _ _ _) (_, _) = initialGrid spaceships      
handleKeys (EventKey (Char '4') _ _ _) (_, _) = initialGrid gosperGliderGun   

-- Pause/Play
handleKeys (EventKey (SpecialKey KeySpace) Up _ _) (grid, True ) = (grid, False)
handleKeys (EventKey (SpecialKey KeySpace) Up _ _) (grid, False) = (grid, True )

-- Mouse
handleKeys (EventKey (MouseButton LeftButton) Down _ (x, y)) (grid, True) = (manualGrid grid coord, True)
    where 
        coord = convertCoord (x, y)

--Other        
handleKeys _ grid = grid

{- Rendering -}

drawCell :: Int -> Picture
drawCell 1 = color (makeColorI 0 206 209 255) $ rectangleSolid cellDimension cellDimension
drawCell 2 = color (makeColorI 255 69 0  255 ) $ rectangleSolid cellDimension cellDimension
drawCell _ =                                    rectangleWire  cellDimension cellDimension

drawGrid :: Grid -> Picture
drawGrid (grid, _) = translate (fromIntegral screenSizeX * (-0.6))
                               (fromIntegral screenSizeY * (-0.5))
                               frame            
        where frame = pictures [
                                translate (fromIntegral x *cellDimension) 
                                          (fromIntegral y *cellDimension) 
                                          (drawCell z) 
                                          | ((x, y), z) <- grid
                               ]
 
playIGameOfLife :: IO ()
playIGameOfLife = do     
        inputGrid <- readFile "src/inputIm.txt"
        let l = columns * length (lines inputGrid)
        let startGrid = if l == gridSize then lines inputGrid 
                    else emptyGrid
        play window background fps (initialGrid startGrid) drawGrid handleKeys update
                    where window = InWindow "Game of Life - The Immigration Game" (screenSizeX, screenSizeY) (50, 50)

background :: Color
background = white

fps :: Int
fps = 3