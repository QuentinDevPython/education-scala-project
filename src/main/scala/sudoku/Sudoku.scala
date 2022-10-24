package sudoku

import scala.io.StdIn.readLine
import constraintLibrary.*


def sudoku(): Unit =
  val difficulty: String = askDifficultySudoku()

  var listSudokuBoxes: List[String] = List[String]()
  for (number <- 11 to 99){
    if (number % 10 != 0) then listSudokuBoxes = listSudokuBoxes ::: List[String](s"v$number")
  }

  var mapBoxWithVariable: Map[String, Variable[Int]] = Map[String, Variable[Int]]()
  for (box <- listSudokuBoxes){
    mapBoxWithVariable = mapBoxWithVariable + (box -> Variable[Int](box))
  }

  launchChosenSudoku(difficulty: String, mapBoxWithVariable: Map[String, Variable[Int]])


def askDifficultySudoku(): String =
  println("Choisissez le niveau du sudoku à résoudre :")
  println("- Facile (F)")
  println("- Medium (M)")
  println("- Hard (H)")
  println("- Impossible (I)")
  String(readLine())

// Sudoku grids on : https://sudoku.com/fr/moyen/
def launchChosenSudoku(difficulty: String, mapBoxWithVariable: Map[String, Variable[Int]]): Unit =

  var constraintGrid: List[Constraint[Int]] = List[Constraint[Int]]()

  // Now we lauch the correct sudoku difficulty
  if (difficulty == "F") {
    println("Résolution du niveau facile ...")
    
    val listBoxes1: List[String] = List[String]("v28", "v35", "v47", "v74")
    val listVariablesSetTo1: List[Constraint[Int]] = equalConstant(listBoxes1: List[String], 1, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo1

    val listBoxes2: List[String] = List[String]("v15", "v57", "v64", "v78", "v86", "v93")
    val listVariablesSetTo2: List[Constraint[Int]] = equalConstant(listBoxes2: List[String], 2, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo2

    val listBoxes3: List[String] = List[String]("v38", "v63", "v72", "v95")
    val listVariablesSetTo3: List[Constraint[Int]] = equalConstant(listBoxes3: List[String], 3, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo3

    val listBoxes4: List[String] = List[String]("v36", "v52", "v65", "v81")
    val listVariablesSetTo4: List[Constraint[Int]] = equalConstant(listBoxes4: List[String], 4, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo4

    val listBoxes5: List[String] = List[String]("v12", "v29", "v34", "v45", "v53")
    val listVariablesSetTo5: List[Constraint[Int]] = equalConstant(listBoxes5: List[String], 5, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo5

    val listBoxes6: List[String] = List[String]("v46")
    val listVariablesSetTo6: List[Constraint[Int]] = equalConstant(listBoxes6: List[String], 6, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo6

    val listBoxes7: List[String] = List[String]("v42", "v76", "v98")
    val listVariablesSetTo7: List[Constraint[Int]] = equalConstant(listBoxes7: List[String], 7, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo7

    val listBoxes8: List[String] = List[String]("v68", "v75", "v82")
    val listVariablesSetTo8: List[Constraint[Int]] = equalConstant(listBoxes8: List[String], 8, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo8

    val listBoxes9: List[String] = List[String]("v17", "v24", "v32", "v58")
    val listVariablesSetTo9: List[Constraint[Int]] = equalConstant(listBoxes9: List[String], 9, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo9
  }
  else if (difficulty == "M") {
    println("Résolution du niveau médium ...")
    
    val listBoxes1: List[String] = List[String]("v13", "v48")
    val listVariablesSetTo1: List[Constraint[Int]] = equalConstant(listBoxes1: List[String], 1, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo1

    val listBoxes2: List[String] = List[String]("v24", "v47", "v62", "v85")
    val listVariablesSetTo2: List[Constraint[Int]] = equalConstant(listBoxes2: List[String], 2, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo2

    val listBoxes3: List[String] = List[String]("v21", "v54", "v69", "v83", "v96")
    val listVariablesSetTo3: List[Constraint[Int]] = equalConstant(listBoxes3: List[String], 3, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo3

    val listBoxes4: List[String] = List[String]("v12", "v25", "v44", "v57", "v93")
    val listVariablesSetTo4: List[Constraint[Int]] = equalConstant(listBoxes4: List[String], 4, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo4

    val listBoxes5: List[String] = List[String]("v68", "v77", "v92")
    val listVariablesSetTo5: List[Constraint[Int]] = equalConstant(listBoxes5: List[String], 5, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo5

    val listBoxes6: List[String] = List[String]("v23", "v59", "v95")
    val listVariablesSetTo6: List[Constraint[Int]] = equalConstant(listBoxes6: List[String], 6, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo6

    val listBoxes7: List[String] = List[String]("v38", "v45", "v51", "v73")
    val listVariablesSetTo7: List[Constraint[Int]] = equalConstant(listBoxes7: List[String], 7, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo7

    val listBoxes8: List[String] = List[String]("v16", "v28")
    val listVariablesSetTo8: List[Constraint[Int]] = equalConstant(listBoxes8: List[String], 8, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo8

    val listBoxes9: List[String] = List[String]("v26", "v75")
    val listVariablesSetTo9: List[Constraint[Int]] = equalConstant(listBoxes9: List[String], 9, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo9
  }
  else if (difficulty == "H") {
    println("Résolution du niveau hard ...")

    val listBoxes1: List[String] = List[String]("v14", "v79")
    val listVariablesSetTo1: List[Constraint[Int]] = equalConstant(listBoxes1: List[String], 1, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo1

    val listBoxes2: List[String] = List[String]("v56", "v94")
    val listVariablesSetTo2: List[Constraint[Int]] = equalConstant(listBoxes2: List[String], 2, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo2

    val listBoxes3: List[String] = List[String]("v32")
    val listVariablesSetTo3: List[Constraint[Int]] = equalConstant(listBoxes3: List[String], 3, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo3

    val listBoxes4: List[String] = List[String]("v38", "v81")
    val listVariablesSetTo4: List[Constraint[Int]] = equalConstant(listBoxes4: List[String], 4, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo4

    val listBoxes5: List[String] = List[String]("v25", "v49")
    val listVariablesSetTo5: List[Constraint[Int]] = equalConstant(listBoxes5: List[String], 5, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo5

    val listBoxes6: List[String] = List[String]("v26", "v43", "v67")
    val listVariablesSetTo6: List[Constraint[Int]] = equalConstant(listBoxes6: List[String], 6, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo6

    val listBoxes7: List[String] = List[String]("v45", "v93", "v86")
    val listVariablesSetTo7: List[Constraint[Int]] = equalConstant(listBoxes7: List[String], 7, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo7

    val listBoxes8: List[String] = List[String]("v46", "v29", "v88")
    val listVariablesSetTo8: List[Constraint[Int]] = equalConstant(listBoxes8: List[String], 8, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo8

    val listBoxes9: List[String] = List[String]("v23", "v61", "v74")
    val listVariablesSetTo9: List[Constraint[Int]] = equalConstant(listBoxes9: List[String], 9, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo9
  }
  else if (difficulty == "I") {
    println("Ca me paraît impossible. Tentative en cours ...")
    
    val listBoxes1: List[String] = List[String]("v28", "v29", "v35", "v47", "v74") //Erreur introduite ici
    val listVariablesSetTo1: List[Constraint[Int]] = equalConstant(listBoxes1: List[String], 1, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo1

    val listBoxes2: List[String] = List[String]("v15", "v57", "v64", "v78", "v86", "v93")
    val listVariablesSetTo2: List[Constraint[Int]] = equalConstant(listBoxes2: List[String], 2, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo2

    val listBoxes3: List[String] = List[String]("v38", "v63", "v72", "v95")
    val listVariablesSetTo3: List[Constraint[Int]] = equalConstant(listBoxes3: List[String], 3, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo3

    val listBoxes4: List[String] = List[String]("v36", "v52", "v65", "v81")
    val listVariablesSetTo4: List[Constraint[Int]] = equalConstant(listBoxes4: List[String], 4, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo4

    val listBoxes5: List[String] = List[String]("v12", "v29", "v34", "v45", "v53")
    val listVariablesSetTo5: List[Constraint[Int]] = equalConstant(listBoxes5: List[String], 5, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo5

    val listBoxes6: List[String] = List[String]("v46")
    val listVariablesSetTo6: List[Constraint[Int]] = equalConstant(listBoxes6: List[String], 6, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo6

    val listBoxes7: List[String] = List[String]("v42", "v76", "v98")
    val listVariablesSetTo7: List[Constraint[Int]] = equalConstant(listBoxes7: List[String], 7, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo7

    val listBoxes8: List[String] = List[String]("v68", "v75", "v82")
    val listVariablesSetTo8: List[Constraint[Int]] = equalConstant(listBoxes8: List[String], 8, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo8

    val listBoxes9: List[String] = List[String]("v17", "v24", "v32", "v58")
    val listVariablesSetTo9: List[Constraint[Int]] = equalConstant(listBoxes9: List[String], 9, mapBoxWithVariable: Map[String, Variable[Int]])
    constraintGrid = constraintGrid ::: listVariablesSetTo9
  }
  else {
    println("Je n'ai pas compris votre choix.")
    val difficulty: String = askDifficultySudoku()
    launchChosenSudoku(difficulty: String, mapBoxWithVariable: Map[String, Variable[Int]])
  }

  val sudokuCsp: CSP[Int] = initializeSudoku(constraintGrid: List[Constraint[Int]], mapBoxWithVariable: Map[String, Variable[Int]])
  solveSudoku(sudokuCsp: CSP[Int])


def initializeSudoku(constraintGrid: List[Constraint[Int]], mapBoxWithVariable: Map[String, Variable[Int]]): CSP[Int] =

  // Possible values than can take a sudoku box
  val numberDomain: Domain[Int] = Domain[Int](Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
  
  var mapBoxWithNumberDomain: Map[Variable[Int], Domain[Int]] = Map()
  for (boxVariable <- mapBoxWithVariable.values){
    mapBoxWithNumberDomain = mapBoxWithNumberDomain + (boxVariable -> numberDomain)
  }

  // Define all the constraints to solve the Sudoku
  val listLinesConstrainsts: List[Constraint[Int]] = allDiffLines(mapBoxWithVariable: Map[String, Variable[Int]])
  val listColumnsConstraints: List[Constraint[Int]] = allDiffColumns(mapBoxWithVariable: Map[String, Variable[Int]])
  val listCellsConstraints: List[Constraint[Int]] = allDiffCells(mapBoxWithVariable: Map[String, Variable[Int]])

  val constraintListSolveSudoku: List[Constraint[Int]] = listLinesConstrainsts ::: listColumnsConstraints ::: listCellsConstraints
  val constraintList: List[Constraint[Int]] = constraintGrid ::: constraintListSolveSudoku


  val sudokuCsp: CSP[Int] =
    CSP(
      domains = mapBoxWithNumberDomain,
      constraints = constraintList
    )
  sudokuCsp


def solveSudoku(sudokuCsp: CSP[Int]): Unit =
  val solvedSudoku: Map[Variable[Int], Domain[Int]] = sudokuCsp.solve

  if solvedSudoku.isEmpty then println("Sudoku impossible à résoudre.\n")
  else

    val nbLines: Int = 9
    val nbColumns: Int = 9

    println("-----------------------------------------")
    
    for (line <- 1 to nbLines) {
      if (line != 1) println()
      print("|| ")
      for (column <- 1 to nbColumns) {
        print(solvedSudoku(Variable(s"v$line$column")).values.head)
        if (column % 3 == 0) print(" || ") else print(" | ")
      }
    }

    println()
    println("-----------------------------------------")


def equalConstant(listSudokuBoxes: List[String], constant: Int, mapBoxWithVariable: Map[String, Variable[Int]]): List[Constraint[Int]] =
  var listEqualConstant: List[Constraint[Int]] = List[Constraint[Int]]()
  for (box <- listSudokuBoxes){
    listEqualConstant = listEqualConstant ::: List[Constraint[Int]](Constraint.EqualConstant(mapBoxWithVariable(box), constant))
  }
  listEqualConstant


def allDiffLineVariables(firstBox: Int, mapBoxWithVariable: Map[String, Variable[Int]]): List[Variable[Int]] =
  var listDiffVariables: List[Variable[Int]] = List[Variable[Int]]()
  for (number <- firstBox to firstBox + 9 - 1) {
    listDiffVariables = listDiffVariables ::: List[Variable[Int]](mapBoxWithVariable(s"v$number"))
  }
  listDiffVariables


def allDiffLines(mapBoxWithVariable: Map[String, Variable[Int]]): List[Constraint[Int]] =
  var listLinesConstrainsts: List[Constraint[Int]] = List[Constraint[Int]]()
  for (line <- 1 to 9) {
    listLinesConstrainsts = listLinesConstrainsts ::: List[Constraint[Int]](Constraint.AllDiff(allDiffLineVariables(line * 10 + 1, mapBoxWithVariable: Map[String, Variable[Int]])))
  }
  listLinesConstrainsts


def allDiffColumnsVariables(firstBox: Int, mapBoxWithVariable: Map[String, Variable[Int]]): List[Variable[Int]] =
  var listDiffVariables: List[Variable[Int]] = List[Variable[Int]]()
  for (number <- 1 to 9) {
    val numberVariable: Int = firstBox + (number - 1) * 10
    listDiffVariables = listDiffVariables ::: List[Variable[Int]](mapBoxWithVariable(s"v$numberVariable"))
  }
  listDiffVariables


def allDiffColumns(mapBoxWithVariable: Map[String, Variable[Int]]): List[Constraint[Int]] =
  var listLinesConstrainsts: List[Constraint[Int]] = List[Constraint[Int]]()
  for (column <- 1 to 9) {
    listLinesConstrainsts = listLinesConstrainsts ::: List[Constraint[Int]](Constraint.AllDiff(allDiffColumnsVariables(column + 10, mapBoxWithVariable: Map[String, Variable[Int]])))
  }
  listLinesConstrainsts


def allDiffCellsVariables(firstBox: Int, mapBoxWithVariable: Map[String, Variable[Int]]): List[Variable[Int]] =
  var listDiffVariables: List[Variable[Int]] = List[Variable[Int]]()
  for (iteration <- 1 to 3) {
    val firstNumber = firstBox + 10 * (iteration - 1)
    for (number <- firstNumber to firstNumber + 2) {
      listDiffVariables = listDiffVariables ::: List[Variable[Int]](mapBoxWithVariable(s"v$number"))
    }
  }
  listDiffVariables

def allDiffCells(mapBoxWithVariable: Map[String, Variable[Int]]): List[Constraint[Int]] =
    var listLinesConstrainsts: List[Constraint[Int]] = List[Constraint[Int]]()
    for (cell <- 1 to 3) {
      listLinesConstrainsts = listLinesConstrainsts ::: List[Constraint[Int]](
        Constraint.AllDiff(allDiffCellsVariables(11 + 3 * (cell - 1), mapBoxWithVariable: Map[String, Variable[Int]]))
      )
    }
    for (cell <- 1 to 3) {
      listLinesConstrainsts = listLinesConstrainsts ::: List[Constraint[Int]](
        Constraint.AllDiff(allDiffCellsVariables(41 + 3 * (cell - 1), mapBoxWithVariable: Map[String, Variable[Int]]))
      )
    }
    for (cell <- 1 to 3) {
      listLinesConstrainsts = listLinesConstrainsts ::: List[Constraint[Int]](
        Constraint.AllDiff(allDiffCellsVariables(71 + 3 * (cell - 1), mapBoxWithVariable: Map[String, Variable[Int]]))
      )
    }
    listLinesConstrainsts