module Main where

import GOL
import UI.NCurses

cross :: Board
cross = [
    ((Position 1 1), Alive)
  , ((Position 2 0), Alive)
  , ((Position 2 1), Alive)
  , ((Position 2 2), Alive)
  , ((Position 3 1), Alive)
  ]

glider :: Board
glider = [
    ((Position 1 1), Alive)
  , ((Position 2 2), Alive)
  , ((Position 3 0), Alive)
  , ((Position 3 1), Alive)
  , ((Position 3 2), Alive)
  ]

inf_one :: Board
inf_one = [
    ((Position 4 4), Alive),((Position 4 5), Alive),((Position 4 6), Alive),((Position 4 8), Alive),
    ((Position 5 4), Alive),
    ((Position 6 7), Alive),((Position 6 8), Alive),
    ((Position 7 6), Alive),((Position 7 6), Alive),((Position 7 8), Alive),
    ((Position 8 4), Alive),((Position 8 6), Alive),((Position 8 8), Alive)
  ]
  
data Game = Game Int Int Board deriving Show

render_game (Game width height board) = mapM_ (render_line board width) [0..height]

render_line board width y = mapM_ (render_cell board y) [0..width]

render_cell :: Board -> Int -> Int -> Update ()
render_cell board y x = do
  moveCursor (toInteger x) (toInteger y)
  drawString cell
  where cell = if (is_alive (get_cell_state board (Position x y))) then "X" else "."
    
update_game (Game width height board) = (Game width height (compute_new_board board width height))

run_game n g = go n g
  where
    go n g | n > 0 = go (n-1) (update_game g)
    go n g         = g


g = Game 20 20 inf_one

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  waitFor g w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Game -> Window -> (Event -> Bool) -> Curses ()
waitFor g w p = loop g where
      loop g' = do
        updateWindow w $ do
          render_game g'
        render
        ev <- getEvent w Nothing
        case ev of
          Nothing -> loop g'
          Just ev' -> if p ev' then return () else loop (update_game g')

