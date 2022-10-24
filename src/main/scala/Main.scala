import scala.io.StdIn.readLine

import sudoku.sudoku
import mapColoring.mapColoring


@main
def main: Unit =

  def launchGameSolver(): Unit =
    val game: String = askGame()
    launchChosenGame(game: String)

  // Demander à l'utilisateur le jeu qu'il souhaite résoudre
  def askGame(): String =
    println("Quel jeu voulez-vous résoudre ?")
    println("- Un sudoku (S)")
    println("- Une carte colorée (C)")
    String(readLine())

  def launchChosenGame(game: String): Unit =
    if (game == "S") {
      println("Welcome to the SUDOKU GAME !")
      sudoku()
    }
    else if(game == "C"){
      println("Welcome to the MAP COLORING GAME !")
      mapColoring()
    }
    else {
      println("Je n'ai pas compris votre choix.")
      val game: String = askGame()
      launchChosenGame(game: String)
    }

  launchGameSolver()