# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
    All_My_Pieces = All_Pieces +
        [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [0, 2]]),
        [[[0, 0],[-1, 0], [1, 0],[2, 0], [3, 0]],
         [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
        rotations([[0, 0], [0, 1], [1, 0]])
    ]
    # your enhancements here

    def self.next_piece (board, cheat = nil)
        if cheat.nil?
            MyPiece.new(All_My_Pieces.sample, board)
        else MyPiece.new(cheat, board)
        end
    end
end

class MyBoard < Board
    def initialize (game)
        super
        @current_block = MyPiece.next_piece(self)
    end

    def rotate_180_degrees
        if !game_over? and @game.is_running?
            @current_block.move(0,0,2)
        end
        draw
    end

    def next_piece
        @current_block = MyPiece.next_piece(self, @cheat_piece)
        @cheat_piece = nil
        @current_pos = nil
    end


    # your enhancements here
    def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size-1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
    end

    def cheat
        if @score >= 100 and @cheat_piece.nil?
            @score -=100
            @game.update_score
            @cheat_piece = Cheat_Piece
        end
    end
    Cheat_Piece = [[[0, 0]]]
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
        @root.bind('c', proc {@board.cheat })
    end
end
