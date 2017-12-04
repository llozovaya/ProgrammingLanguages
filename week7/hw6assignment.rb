# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
    All_My_Pieces = All_Pieces + 
        [rotations([[0, 0], [1, 0], [0,1], [1, 1], [2, 0]]),
        [[[0, 0],[-1, 0], [1, 0],[2, 0], [3, 0]],
         [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
        rotations([[0, 0], [0, 1], [1, 0]])
    ]
    # your enhancements here
    def self.next_piece (board)
        MyPiece.new(All_My_Pieces.sample, board)
    end
end

class MyBoard < Board
    def rotate_180_degrees
        if !game_over? and @game.is_running?
            @current_block.move(0,0,2)
        end
        draw
    end

    def next_piece
        @current_block = MyPiece.next_piece(self)
        @current_pos = nil
    end
  # your enhancements here
end

class MyTetris < Tetris
    def set_board
        @canvas = TetrisCanvas.new
        @board = MyBoard.new(self)
        @canvas.place(@board.block_size * @board.num_rows + 3,
                     @board.block_size * @board.num_columns + 6, 24, 80)
        @board.draw

    end
  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180_degrees})
  end

end

