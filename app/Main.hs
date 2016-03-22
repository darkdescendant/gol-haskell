module Main where

import GOL
import UI.NCurses

cross :: Board
cross = make_board [(1,1),(2,0),(2,1),(2,2),(3,1)]

glider :: Board
glider = make_board [(1,1),(2,2),(3,0),(3,1),(3,2)]

inf_one :: Board
inf_one = make_board [(4,4),(4,5),(4,6),(4,8),(5,4),(6,7),(6,8),(7,6),(7,6),(7,8),(8,4),(8,6),(8,8)]


render_game (Game width height board) = mapM_ (render_line board width) [0..height]

render_line board width y = mapM_ (render_cell board y) [0..width]

render_cell :: Board -> Int -> Int -> Update ()
render_cell board y x = do
  moveCursor (toInteger x) (toInteger y)
  drawString cell
  where cell = if (is_alive (get_cell_state board (Position x y))) then "X" else "."
    
run_game n g = go n g
  where
    go n g | n > 0 = go (n-1) (update_game g)
    go n g         = g


g = Game 20 20 cross

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

