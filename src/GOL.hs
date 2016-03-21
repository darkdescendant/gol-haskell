module GOL where

import Data.List

data CellState = Alive | Dead
    deriving (Show, Eq)

data Position = Position Int Int
    deriving (Show, Eq)

type Board = [(Position, CellState)]

get_cell_state :: Board -> Position -> CellState
get_cell_state board position =
  case (lookup position board) of
  Just cs -> cs
  Nothing -> Dead

cell_neighbors :: Position -> [Position]
cell_neighbors (Position x y) = [(Position a b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], a >= 0 && b >= 0 && not (a==x && b==y)]

is_alive :: CellState -> Bool
is_alive Alive = True
is_alive Dead = False

alive_neighbor_cells :: Board -> Position -> Int
alive_neighbor_cells board position = length $ filter is_alive $ map (get_cell_state board) $ cell_neighbors position

next_cell_state :: Board -> Position -> CellState
next_cell_state board position =
  case (alive_neighbor_cells board position) of
    2 -> get_cell_state board position
    3 -> Alive
    _ -> Dead

compute_new_board :: Board -> Int -> Int -> Board
compute_new_board board height width =
  concat $ map (compute_new_line board width) [0..height]

compute_new_line :: Board -> Int -> Int -> Board
compute_new_line board width y =
  concat $ map (compute_new_cell board y) [0..width]

compute_new_cell :: Board -> Int -> Int -> Board
compute_new_cell board y x =
  case (next_cell_state board pos) of
    Alive -> [(pos, Alive)]
    Dead -> []
  where pos = (Position x y)

print_board board width height = map (print_line board width) [0..height]

print_line board width y = concat $ map (print_cell board y) [0..width]

print_cell board y x =
  case (get_cell_state board (Position x y)) of
    Alive -> ['X']
    Dead  -> [' ']  
