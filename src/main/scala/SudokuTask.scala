import SudokuBoard.*
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.parallel.CollectionConverters.*

object SudokuTask {

  def solve(input: String, board: Board): Board = {
    // Adding the input into the board
    def parse_input(input: String) = {
      List.tabulate(81)(_ + 0).par.foreach(index => {
        val data: String = if input(index) == '.' then "123456789" else input(index).toString
        board.squares(index).setData(data)
      })
    }

    // Check if it's solved
    def is_solved(): Boolean = {board.squares.par.count(square => square.getData.length == 1) == 81}

    // Try all the values in one square
    def try_square(square: Square, saved_values: mutable.HashMap[Square, String]): Boolean = {
      var clean_run = false
      square.getData.foreach(x => {
        // If the value worked then do nothing else reset it back to saved point and try next value
        if clean_run then () else { saved_values.par.foreach(item => item._1.setData(item._2))
          square.setData(x.toString)
          clean_run = eliminateAll() }
      })
      if !clean_run then saved_values.par.foreach(item => item._1.setData(item._2))
      clean_run
    }

    // Try every single square that I need to
    def try_all_squares(filtered: Vector[Square], saved_value: mutable.HashMap[Square, String], index: Int): Boolean = {
      if is_solved() || index >= filtered.size then true else {
        if try_square(filtered(index), saved_value) then {
          val filtered_by_lowest = board.squares.par.filter(_.getData.length > 1).seq.sortBy(_.getData.length)
          val saved_values_new: mutable.HashMap[Square, String] = new mutable.HashMap[Square, String]()
          // Cannot parallelize this since we actually need to try the box in the proper order or everything will break
          filtered_by_lowest.foreach(square => saved_values_new.put(square, square.getData))
          try_all_squares(filtered_by_lowest, saved_values_new, 0)
        }
        else {
          try_all_squares(filtered, saved_value, index + 1)
        }
        false
      }
    }

    // If a number found in a square then eliminate that number from all it's peers
    def eliminatePeers(): Boolean = {
      var clean_run = true
      board.squares.par.foreach(square => {
        val size = square.getData.length
        if size == 1 then square.getPeers.par.foreach(peer =>
          if peer.getData.contains(square.getData) && peer.getData.length == 1
          then clean_run = false else peer.removeData(square.getData))
      })
      clean_run
    }

    // If a unit has a unique value then map it to the square that contains that unique value
    def eliminateAllUnit(): Boolean = {
      var clean_run = false
      def eliminateOneUnit(unit: Vector[SudokuBoard.Square]) = {
        val countMap: mutable.HashMap[Char, Int] = new mutable.HashMap()
        unit.par.foreach(square => square.getData.foreach(char => countMap.update(char, countMap.getOrElse(char, 0)+1)))
        val countMap2: mutable.HashMap[Char, Int] = countMap.filter((_, int) => int==1)
        unit.par.foreach(square => countMap2.keys.foreach(char => if square.getData.contains(char)
        then {if square.getData != char.toString then clean_run = true; square.setData(char.toString) }))
      }
      board.box_group.par.foreach(unit => eliminateOneUnit(unit))
      board.row_group.par.foreach(unit => eliminateOneUnit(unit))
      board.col_group.par.foreach(unit => eliminateOneUnit(unit))
      clean_run
    }

    // Uses all my elimination techniques
    def eliminateAll(): Boolean = {
      var clean_run: Boolean = true
      var beforeEliminate: String = ""
      var afterEliminate: String = ""
      board.squares.par.map(square => square.getData).par.foreach(data => afterEliminate+=data)
      while (beforeEliminate != afterEliminate) {
        if eliminatePeers() || eliminateAllUnit() then clean_run = true else clean_run = false
        beforeEliminate = afterEliminate
        afterEliminate = ""
        board.squares.map(square => square.getData).par.foreach(data => afterEliminate+=data)
      }
      clean_run
    }


    parse_input(input)
    eliminateAll()
    // Saving the values that we will change by trying out brute force method
    val filtered_by_lowest = board.squares.filter(_.getData.length > 1).sortBy(_.getData.length)
    val saved_values: mutable.HashMap[Square, String] = new mutable.HashMap[Square, String]()
    filtered_by_lowest.foreach(square => saved_values.put(square, square.getData))
    try_all_squares(filtered_by_lowest, saved_values, 0)


    board
  }


  def print_board(board: Board) = {
    List.tabulate(81)(_ + 0).foreach(index => {
      if index % 27 == 0 then println()
      if index % 9 == 0 then  println() else
      if index % 3 == 0 then print(". ")
      print(board.squares(index).getData)
      print(" ")
    })
  }

}

