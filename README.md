# Sudoku Solver Project
Project Consists of 3 Scala files and a pdf describing the process of making this project\
Main.scala -> Entrypoint of the project\
SudokuTask.scala -> Where everything is done\
SudokuBoard.scala -> A class containing the details of a simple sudoku board

### Fixed Goals:
1. Given a new sudoku board with a guaranteed possible solution, to be able to keep track of the possible values in each cell of the board according to the 3x3 grid surrounding the board, the row and the column the cell resides in.
2. To collect and discern the values in each cell and filter it in a way which the algorithm can make use of.
3. To successfully be able to apply the following algorithm to each cell until a board is completely filled up.
4. In case the algorithm doesn’t work anywhere, to have a backup brute force method to guarantee a solution albeit a slow one

#### Naphong Chadha 6380797