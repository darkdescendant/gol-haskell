import Test.Hspec
import Test.QuickCheck

import GOL

board :: Board
board = [((Position 1 2), Alive)]

bar :: Board
bar = [
    ((Position 1 1), Alive)
  , ((Position 1 2), Alive)
  , ((Position 1 3), Alive)
  ]

bar2 :: Board
bar2 = [
    ((Position 0 2), Alive)
  , ((Position 1 2), Alive)
  , ((Position 2 2), Alive)
  ]

cross :: Board
cross = [
    ((Position 1 1), Alive)
  , ((Position 2 0), Alive)
  , ((Position 2 1), Alive)
  , ((Position 2 2), Alive)
  , ((Position 3 1), Alive)
  ]
  
main :: IO ()
main = hspec $ do
  describe "Game of Life" $ do
    it "Should return Dead CellState for missing Position" $ do
      (get_cell_state board (Position 1 1)) `shouldBe` Dead

    it "Should return Alive for a known Position" $ do
      (get_cell_state board (Position 1 2)) `shouldBe` Alive

    it "Should have 8 neighbors for internal cell" $ do
      (cell_neighbors (Position 1 1)) `shouldBe` [(Position 0 0),(Position 0 1),(Position 0 2),(Position 1 0),(Position 1 2),(Position 2 0),(Position 2 1),(Position 2 2)]

    it "Should have 3 neighbors for edge cell" $ do
      (cell_neighbors (Position 0 0)) `shouldBe` [(Position 0 1),(Position 1 0),(Position 1 1)]

    it "Should return return 0 Alive neighbors for single cell" $ do
      (alive_neighbor_cells board (Position 1 2)) `shouldBe` 0

    it "Should return return 2 Alive neighbors for bar middle cell" $ do
      (alive_neighbor_cells board (Position 1 2)) `shouldBe` 0
      
    it "Should kill a cell if it is alone" $ do
      (next_cell_state board (Position 1 1)) `shouldBe` Dead
            
    it "Should keep Alive cell state if it has two neighbors" $ do
      (next_cell_state bar (Position 1 2)) `shouldBe` Alive
            
    it "Should keep Dead cell Dead if it has two neighbors" $ do
      (next_cell_state bar (Position 0 0)) `shouldBe` Dead
            
    it "Should Change Alivee cell state to Dead if it has > three neighbors" $ do
      (next_cell_state cross (Position 2 1)) `shouldBe` Dead
            
    it "Should Change Dead cell state to Alive if it has three neighbors" $ do
      (next_cell_state cross (Position 0 0)) `shouldBe` Dead

    it "Should compute new bar state from bar board" $ do
      (compute_new_board bar 10 10) `shouldBe` bar2
