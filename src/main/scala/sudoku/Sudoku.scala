package sudoku

import scala.io.StdIn.readLine
import scala.collection.mutable.ListBuffer
import constraintLibrary.*


// Here we declare all the sudoku boxes as variables

// 1st cell
val v11 = Variable[Int]("v11")
val v12 = Variable[Int]("v12")
val v13 = Variable[Int]("v13")

val v21 = Variable[Int]("v21")
val v22 = Variable[Int]("v22")
val v23 = Variable[Int]("v23")

val v31 = Variable[Int]("v31")
val v32 = Variable[Int]("v32")
val v33 = Variable[Int]("v33")

// 2nd cell
val v14 = Variable[Int]("v14")
val v15 = Variable[Int]("v15")
val v16 = Variable[Int]("v16")

val v24 = Variable[Int]("v24")
val v25 = Variable[Int]("v25")
val v26 = Variable[Int]("v26")

val v34 = Variable[Int]("v34")
val v35 = Variable[Int]("v35")
val v36 = Variable[Int]("v36")

// 3rd cell
val v17 = Variable[Int]("v17")
val v18 = Variable[Int]("v18")
val v19 = Variable[Int]("v19")

val v27 = Variable[Int]("v27")
val v28 = Variable[Int]("v28")
val v29 = Variable[Int]("v29")

val v37 = Variable[Int]("v37")
val v38 = Variable[Int]("v38")
val v39 = Variable[Int]("v39")

// 4th cell
val v41 = Variable[Int]("v41")
val v42 = Variable[Int]("v42")
val v43 = Variable[Int]("v43")

val v51 = Variable[Int]("v51")
val v52 = Variable[Int]("v52")
val v53 = Variable[Int]("v53")

val v61 = Variable[Int]("v61")
val v62 = Variable[Int]("v62")
val v63 = Variable[Int]("v63")

// 5th cell
val v44 = Variable[Int]("v44")
val v45 = Variable[Int]("v45")
val v46 = Variable[Int]("v46")

val v54 = Variable[Int]("v54")
val v55 = Variable[Int]("v55")
val v56 = Variable[Int]("v56")

val v64 = Variable[Int]("v64")
val v65 = Variable[Int]("v65")
val v66 = Variable[Int]("v66")

// 6th cell
val v47 = Variable[Int]("v47")
val v48 = Variable[Int]("v48")
val v49 = Variable[Int]("v49")

val v57 = Variable[Int]("v57")
val v58 = Variable[Int]("v58")
val v59 = Variable[Int]("v59")

val v67 = Variable[Int]("v67")
val v68 = Variable[Int]("v68")
val v69 = Variable[Int]("v69")

// 7th cell
val v71 = Variable[Int]("v71")
val v72 = Variable[Int]("v72")
val v73 = Variable[Int]("v73")

val v81 = Variable[Int]("v81")
val v82 = Variable[Int]("v82")
val v83 = Variable[Int]("v83")

val v91 = Variable[Int]("v91")
val v92 = Variable[Int]("v92")
val v93 = Variable[Int]("v93")

// 8th cell
val v74 = Variable[Int]("v74")
val v75 = Variable[Int]("v75")
val v76 = Variable[Int]("v76")

val v84 = Variable[Int]("v84")
val v85 = Variable[Int]("v85")
val v86 = Variable[Int]("v86")

val v94 = Variable[Int]("v94")
val v95 = Variable[Int]("v95")
val v96 = Variable[Int]("v96")

// 9th cell
val v77 = Variable[Int]("v77")
val v78 = Variable[Int]("v78")
val v79 = Variable[Int]("v79")

val v87 = Variable[Int]("v87")
val v88 = Variable[Int]("v88")
val v89 = Variable[Int]("v89")

val v97 = Variable[Int]("v97")
val v98 = Variable[Int]("v98")
val v99 = Variable[Int]("v99")

def sudoku(): Unit =
  val difficulty = askDifficultySudoku()
  launchChosenSudoku(difficulty)

def askDifficultySudoku(): String =
  println("Choisissez le niveau du sudoku à résoudre :")
  println("- Facile (F)")
  println("- Medium (M)")
  println("- Hard (H)")
  println("- Impossible (I)")
  
  String(readLine())

// Sudoku grids on : https://sudoku.com/fr/moyen/
def launchChosenSudoku(difficulty: String): Unit =

  // Now we lauch the correct sudoku difficulty
  if (difficulty == "F") {
    println("Résolution du niveau facile ...")

    val constraintGridEasy: List[Constraint[Int]] =
      List(
        // All the 1 values
        Constraint.EqualConstant(v28, 1),
        Constraint.EqualVariables(v28, v35),
        Constraint.EqualVariables(v28, v47),
        Constraint.EqualVariables(v28, v74),

        // All the 2 values
        Constraint.EqualConstant(v15, 2),
        Constraint.EqualVariables(v15, v57),
        Constraint.EqualVariables(v15, v64),
        Constraint.EqualVariables(v15, v78),
        Constraint.EqualVariables(v15, v86),
        Constraint.EqualVariables(v15, v93),

        // All the 3 values
        Constraint.EqualConstant(v38, 3),
        Constraint.EqualVariables(v38, v63),
        Constraint.EqualVariables(v38, v72),
        Constraint.EqualVariables(v38, v95),

        // All the 4 values
        Constraint.EqualConstant(v36, 4),
        Constraint.EqualVariables(v36, v52),
        Constraint.EqualVariables(v36, v65),
        Constraint.EqualVariables(v36, v81),

        // All the 5 values
        Constraint.EqualConstant(v12, 5),
        Constraint.EqualVariables(v12, v29),
        Constraint.EqualVariables(v12, v34),
        Constraint.EqualVariables(v12, v45),
        Constraint.EqualVariables(v12, v53),

        // All the 6 values
        Constraint.EqualConstant(v46, 6),

        // All the 7 values
        Constraint.EqualConstant(v42, 7),
        Constraint.EqualVariables(v42, v76),
        Constraint.EqualVariables(v42, v98),

        // All the 8 values
        Constraint.EqualConstant(v68, 8),
        Constraint.EqualVariables(v68, v75),
        Constraint.EqualVariables(v68, v82),

        // All the 9 values
        Constraint.EqualConstant(v17, 9),
        Constraint.EqualVariables(v17, v24),
        Constraint.EqualVariables(v17, v32),
        Constraint.EqualVariables(v17, v58)
      )

    val sudokuCsp: CSP[Int] = initializeSudoku(constraintGridEasy)
    solveSudoku(sudokuCsp: CSP[Int])
  }
  else if (difficulty == "M") {
    println("Résolution du niveau médium ...")

    val constraintGridMedium: List[Constraint[Int]] =
      List(
        // All the 1 values
        Constraint.EqualConstant(v13, 1),
        Constraint.EqualVariables(v13, v48),

        // All the 2 values
        Constraint.EqualConstant(v24, 2),
        Constraint.EqualVariables(v24, v47),
        Constraint.EqualVariables(v24, v62),
        Constraint.EqualVariables(v24, v85),

        // All the 3 values
        Constraint.EqualConstant(v21, 3),
        Constraint.EqualVariables(v21, v54),
        Constraint.EqualVariables(v21, v69),
        Constraint.EqualVariables(v21, v83),
        Constraint.EqualVariables(v21, v96),

        // All the 4 values
        Constraint.EqualConstant(v12, 4),
        Constraint.EqualVariables(v12, v25),
        Constraint.EqualVariables(v12, v44),
        Constraint.EqualVariables(v12, v57),
        Constraint.EqualVariables(v12, v93),

        // All the 5 values
        Constraint.EqualConstant(v68, 5),
        Constraint.EqualVariables(v68, v77),
        Constraint.EqualVariables(v68, v92),

        // All the 6 values
        Constraint.EqualConstant(v23, 6),
        Constraint.EqualVariables(v23, v59),
        Constraint.EqualVariables(v23, v95),

        // All the 7 values
        Constraint.EqualConstant(v38, 7),
        Constraint.EqualVariables(v38, v45),
        Constraint.EqualVariables(v38, v51),
        Constraint.EqualVariables(v38, v73),

        // All the 8 values
        Constraint.EqualConstant(v16, 8),
        Constraint.EqualVariables(v16, v28),

        // All the 9 values
        Constraint.EqualConstant(v26, 9),
        Constraint.EqualVariables(v26, v75),

      )

    val sudokuCsp: CSP[Int] = initializeSudoku(constraintGridMedium)
    solveSudoku(sudokuCsp: CSP[Int])

  }
  else if (difficulty == "I") {
    println("Ca me paraît impossible. Tentative en cours ...")

    val constraintGridImpossible: List[Constraint[Int]] =
      List(
        // All the 1 values
        Constraint.EqualConstant(v28, 1),
        Constraint.EqualVariables(v28, v29), // Impossible d'avoir deux mêmes valeurs sur une ligne
        Constraint.EqualVariables(v28, v35),
        Constraint.EqualVariables(v28, v47),
        Constraint.EqualVariables(v28, v74),

        // All the 2 values
        Constraint.EqualConstant(v15, 2),
        Constraint.EqualVariables(v15, v57),
        Constraint.EqualVariables(v15, v64),
        Constraint.EqualVariables(v15, v78),
        Constraint.EqualVariables(v15, v86),
        Constraint.EqualVariables(v15, v93),

        // All the 3 values
        Constraint.EqualConstant(v38, 3),
        Constraint.EqualVariables(v38, v63),
        Constraint.EqualVariables(v38, v72),
        Constraint.EqualVariables(v38, v95),

        // All the 4 values
        Constraint.EqualConstant(v36, 4),
        Constraint.EqualVariables(v36, v52),
        Constraint.EqualVariables(v36, v65),
        Constraint.EqualVariables(v36, v81),

        // All the 5 values
        Constraint.EqualConstant(v12, 5),
        Constraint.EqualVariables(v12, v29),
        Constraint.EqualVariables(v12, v34),
        Constraint.EqualVariables(v12, v45),
        Constraint.EqualVariables(v12, v53),

        // All the 6 values
        Constraint.EqualConstant(v46, 6),

        // All the 7 values
        Constraint.EqualConstant(v42, 7),
        Constraint.EqualVariables(v42, v76),
        Constraint.EqualVariables(v42, v98),

        // All the 8 values
        Constraint.EqualConstant(v68, 8),
        Constraint.EqualVariables(v68, v75),
        Constraint.EqualVariables(v68, v82),

        // All the 9 values
        Constraint.EqualConstant(v17, 9),
        Constraint.EqualVariables(v17, v24),
        Constraint.EqualVariables(v17, v32),
        Constraint.EqualVariables(v17, v58)
      )

    val sudokuCsp: CSP[Int] = initializeSudoku(constraintGridImpossible)
    solveSudoku(sudokuCsp: CSP[Int])

  }
  else if (difficulty == "H") {
    val constraintGridHard: List[Constraint[Int]] =
      List(
        // All the 1 values
        Constraint.EqualConstant(v14, 1),
        Constraint.EqualConstant(v79, 1),
        Constraint.EqualConstant(v79, 1),

        // All the 2 values
        Constraint.EqualConstant(v56, 2),
        Constraint.EqualConstant(v94, 2),

        // All the 3 values
        Constraint.EqualConstant(v32, 3),

        // All the 4 values
        Constraint.EqualConstant(v38, 4),
        Constraint.EqualConstant(v81, 4),

        // All the 5 values
        Constraint.EqualConstant(v25, 5),
        Constraint.EqualConstant(v49, 5),

        // All the 6 values
        Constraint.EqualConstant(v26, 6),
        Constraint.EqualConstant(v43, 6),
        Constraint.EqualConstant(v67, 6),

        // All the 7 values
        Constraint.EqualConstant(v45, 7),
        Constraint.EqualConstant(v93, 7),
        Constraint.EqualConstant(v86, 7),
        

        // All the 8 values
        Constraint.EqualConstant(v46, 8),
        Constraint.EqualConstant(v29, 8),
        Constraint.EqualConstant(v88, 8),

        // All the 9 values
        Constraint.EqualConstant(v23, 9),
        Constraint.EqualConstant(v61, 9),
        Constraint.EqualConstant(v74, 9),
      )

    val sudokuCsp: CSP[Int] = initializeSudoku(constraintGridHard)
    solveSudoku(sudokuCsp: CSP[Int])
  }
  else {
    println("Je n'ai pas compris votre choix.")
    val difficulty: String = askDifficultySudoku()
    launchChosenSudoku(difficulty)
  }

def initializeSudoku(constraintGrid: List[Constraint[Int]]): CSP[Int] =

  // Sudoku implementation

  // Possible values than can take a sudoku box
  val possibilitiesDomain: Domain[Int] = Domain[Int](Set(1, 2, 3, 4, 5, 6, 7, 8, 9))

  val constraintListSolveSudoku: List[Constraint[Int]] =
    List(
      // AllDiff on sudoku lines
      Constraint.AllDiff(List[Variable[Int]](v11, v12, v13, v14, v15, v16, v17, v18, v19)),
      Constraint.AllDiff(List[Variable[Int]](v21, v22, v23, v24, v25, v26, v27, v28, v29)),
      Constraint.AllDiff(List[Variable[Int]](v31, v32, v33, v34, v35, v36, v37, v38, v39)),
      Constraint.AllDiff(List[Variable[Int]](v41, v42, v43, v44, v45, v46, v47, v48, v49)),
      Constraint.AllDiff(List[Variable[Int]](v51, v52, v53, v54, v55, v56, v57, v58, v59)),
      Constraint.AllDiff(List[Variable[Int]](v61, v62, v63, v64, v65, v66, v67, v68, v69)),
      Constraint.AllDiff(List[Variable[Int]](v71, v72, v73, v74, v75, v76, v77, v78, v79)),
      Constraint.AllDiff(List[Variable[Int]](v81, v82, v83, v84, v85, v86, v87, v88, v89)),
      Constraint.AllDiff(List[Variable[Int]](v91, v92, v93, v94, v95, v96, v97, v98, v99)),

      // AllDiff in sudoku columns
      Constraint.AllDiff(List[Variable[Int]](v11, v21, v31, v41, v51, v61, v71, v81, v91)),
      Constraint.AllDiff(List[Variable[Int]](v12, v22, v32, v42, v52, v62, v72, v82, v92)),
      Constraint.AllDiff(List[Variable[Int]](v13, v23, v33, v43, v53, v63, v73, v83, v93)),
      Constraint.AllDiff(List[Variable[Int]](v14, v24, v34, v44, v54, v64, v74, v84, v94)),
      Constraint.AllDiff(List[Variable[Int]](v15, v25, v35, v45, v55, v65, v75, v85, v95)),
      Constraint.AllDiff(List[Variable[Int]](v16, v26, v36, v46, v56, v66, v76, v86, v96)),
      Constraint.AllDiff(List[Variable[Int]](v17, v27, v37, v47, v57, v67, v77, v87, v97)),
      Constraint.AllDiff(List[Variable[Int]](v18, v28, v38, v48, v58, v68, v78, v88, v98)),
      Constraint.AllDiff(List[Variable[Int]](v19, v29, v39, v49, v59, v69, v79, v89, v99)),

      // AllDiff in sudoku cells
      Constraint.AllDiff(List[Variable[Int]](v11, v12, v13, v21, v22, v23, v31, v32, v33)),
      Constraint.AllDiff(List[Variable[Int]](v14, v15, v16, v24, v25, v26, v34, v35, v36)),
      Constraint.AllDiff(List[Variable[Int]](v17, v18, v19, v27, v28, v29, v37, v38, v39)),

      Constraint.AllDiff(List[Variable[Int]](v41, v42, v43, v51, v52, v53, v61, v62, v63)),
      Constraint.AllDiff(List[Variable[Int]](v44, v45, v46, v54, v55, v56, v64, v65, v66)),
      Constraint.AllDiff(List[Variable[Int]](v47, v48, v49, v57, v58, v59, v67, v68, v69)),

      Constraint.AllDiff(List[Variable[Int]](v71, v72, v73, v81, v82, v83, v91, v92, v93)),
      Constraint.AllDiff(List[Variable[Int]](v74, v75, v76, v84, v85, v86, v94, v95, v96)),
      Constraint.AllDiff(List[Variable[Int]](v77, v78, v79, v87, v88, v89, v97, v98, v99))
    )

  val constraintList: List[Constraint[Int]] = constraintGrid ::: constraintListSolveSudoku


  val sudokuCsp: CSP[Int] =
    CSP(
      domains =
      // Each variable is bound to the same domain
        Map(
          v11 -> possibilitiesDomain,
          v12 -> possibilitiesDomain,
          v13 -> possibilitiesDomain,
          v14 -> possibilitiesDomain,
          v15 -> possibilitiesDomain,
          v16 -> possibilitiesDomain,
          v17 -> possibilitiesDomain,
          v18 -> possibilitiesDomain,
          v19 -> possibilitiesDomain,

          v21 -> possibilitiesDomain,
          v22 -> possibilitiesDomain,
          v23 -> possibilitiesDomain,
          v24 -> possibilitiesDomain,
          v25 -> possibilitiesDomain,
          v26 -> possibilitiesDomain,
          v27 -> possibilitiesDomain,
          v28 -> possibilitiesDomain,
          v29 -> possibilitiesDomain,

          v31 -> possibilitiesDomain,
          v32 -> possibilitiesDomain,
          v33 -> possibilitiesDomain,
          v34 -> possibilitiesDomain,
          v35 -> possibilitiesDomain,
          v36 -> possibilitiesDomain,
          v37 -> possibilitiesDomain,
          v38 -> possibilitiesDomain,
          v39 -> possibilitiesDomain,

          v41 -> possibilitiesDomain,
          v42 -> possibilitiesDomain,
          v43 -> possibilitiesDomain,
          v44 -> possibilitiesDomain,
          v45 -> possibilitiesDomain,
          v46 -> possibilitiesDomain,
          v47 -> possibilitiesDomain,
          v48 -> possibilitiesDomain,
          v49 -> possibilitiesDomain,

          v51 -> possibilitiesDomain,
          v52 -> possibilitiesDomain,
          v53 -> possibilitiesDomain,
          v54 -> possibilitiesDomain,
          v55 -> possibilitiesDomain,
          v56 -> possibilitiesDomain,
          v57 -> possibilitiesDomain,
          v58 -> possibilitiesDomain,
          v59 -> possibilitiesDomain,

          v61 -> possibilitiesDomain,
          v62 -> possibilitiesDomain,
          v63 -> possibilitiesDomain,
          v64 -> possibilitiesDomain,
          v65 -> possibilitiesDomain,
          v66 -> possibilitiesDomain,
          v67 -> possibilitiesDomain,
          v68 -> possibilitiesDomain,
          v69 -> possibilitiesDomain,

          v71 -> possibilitiesDomain,
          v72 -> possibilitiesDomain,
          v73 -> possibilitiesDomain,
          v74 -> possibilitiesDomain,
          v75 -> possibilitiesDomain,
          v76 -> possibilitiesDomain,
          v77 -> possibilitiesDomain,
          v78 -> possibilitiesDomain,
          v79 -> possibilitiesDomain,

          v81 -> possibilitiesDomain,
          v82 -> possibilitiesDomain,
          v83 -> possibilitiesDomain,
          v84 -> possibilitiesDomain,
          v85 -> possibilitiesDomain,
          v86 -> possibilitiesDomain,
          v87 -> possibilitiesDomain,
          v88 -> possibilitiesDomain,
          v89 -> possibilitiesDomain,

          v91 -> possibilitiesDomain,
          v92 -> possibilitiesDomain,
          v93 -> possibilitiesDomain,
          v94 -> possibilitiesDomain,
          v95 -> possibilitiesDomain,
          v96 -> possibilitiesDomain,
          v97 -> possibilitiesDomain,
          v98 -> possibilitiesDomain,
          v99 -> possibilitiesDomain,
        ),
      constraints = constraintList
    )
  sudokuCsp

def solveSudoku(sudokuCsp: CSP[Int]): Unit =
  val solvedSudoku: Map[Variable[Int], Domain[Int]] = sudokuCsp.solve

  if solvedSudoku.isEmpty then println("Sudoku impossible à résoudre.\n")
  else

    val nb_lines: Int = 9
    val nb_columns: Int = 9

    println("-----------------------------------------")
    
    for (x <- 1 to nb_lines) {
      if (x != 1) println()
      print("|| ")
      for (y <- 1 to nb_columns) {
        print(solvedSudoku(Variable(s"v$x$y")).values.head)
        if (y % 3 == 0) print(" || ") else print(" | ")
      }
    }

    println()
    println("-----------------------------------------")
