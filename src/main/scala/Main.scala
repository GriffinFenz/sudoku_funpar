import SudokuBoard.*
import SudokuTask.*

object Main {
  def main(args: Array[String]): Unit = {

    val sudoku_board: Board = new SudokuBoard.Board
    val input_board1: String = "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."
    val input_board2: String = "......................1.627.61.842.9..759...442..6378..38657.1264.3.1978172.4..5."
    val solved_board: Board = SudokuTask.solve(input_board2, sudoku_board)
    SudokuTask.print_board(solved_board)

  }
}
