import qualified Data.Text as Text
import Control.Concurrent
import System.Console.ANSI

-- Helper functions

data State = Alive|Dead deriving (Eq) 
type Cell = ((Int, Int), State)
type Coordinate = (Int,Int)
type Grid = [Cell]

-- globals
width :: Int
width = 30

height :: Int
height = 10

-- Initiates the grid with all states equal to Dead except the passed ones
initGrid :: [Coordinate] -> Grid
initGrid coords = [((u,v), determineState u v) | u <- [0..height], v <- [0..width]]
        where determineState u v
               | (u,v) `elem` coords = Alive
               | otherwise           = Dead


-- Gets the cell of the given position
getAt :: Coordinate -> Grid -> Cell
getAt (x,y) grid =
    grid !! (x * (1 + width) + y)

-- All neighbours withing the bounds
neighbours (x, y) =
    filter (\(u,v) -> (u,v)/=(x,y) && u >= 0 && v >= 0 && u <= height && v <= width)
        [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]

-- Gets the state of the given position
getStateAt :: Coordinate -> Grid -> State
getStateAt pos grid = s
    where ((_,_),s) = getAt pos grid


-- determines if the box is alive or dead in the next generation
aliveNeighbours :: Cell -> Grid -> Int
aliveNeighbours pos grid = length $  filter (\el -> Alive == (getStateAt el grid)) $ neighbours pos

nextState :: Cell -> Grid -> State
nextState ((x,y),state) grid
    | 2 == countAlive && Alive == state = Alive
    | 3 == countAlive = Alive
    | otherwise       = Dead
    where countAlive = aliveNeighbours (x,y) grid

-- this evolves the grid with one generation
update :: Grid -> Grid
update grid = map 
                 (\cell@((x,y),s) -> ((x,y), (nextState cell grid)) ) 
                 grid 

-- For this I took inspiration from an implementation of the game I found on the internet:
-- http://codereview.stackexchange.com/questions/39170/game-of-life-implementation-in-haskell 
toString :: Grid -> Text.Text
toString grid = Text.intercalate (Text.singleton '\n') (rows 0)
  where
    rows x
      | x > height = []
      | otherwise = (row x 0) : rows (x + 1)
    row x y
      | y > width = Text.empty
      | otherwise = Text.cons (stateToChar $ getStateAt (x,y) grid) (row x (y + 1))
    stateToChar state
      | state == Alive = '@'
      | otherwise = '.'

-- Different grids
glider::Grid
glider = initGrid [(8,17),(8,18),(8,19),(9,17),(10,18)]

beacon::Grid
beacon = initGrid [(5,5),(5,6),(6,5),(6,6),(7,7),(7,8),(8,7),(8,8)]

toad::Grid
toad = initGrid [(5,5),(5,6),(5,7),(6,6),(6,7),(6,8)]


draw :: Grid -> IO()
draw grid = do
  let updatedGrid = update grid
  -- clearscreen after the update so to minimize the time between clearing and drawing
  clearScreen
  putStr . Text.unpack $ (toString updatedGrid)
  putStr "\n\n"
  threadDelay 300000
  draw updatedGrid

del :: IO()
del = do
  print "HI"
  threadDelay 100000
  del

main :: IO()
main = do
    putStr "_ _ _ _ _ _ _ _ _ _ \n\n"
