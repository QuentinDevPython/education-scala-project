import scala.io.StdIn.readLine

import constraintLibrary.*
import sudoku.sudoku
import mapColoring.map_coloring

@main
def main: Unit =

  def launch_game_solver() =
    val game: String = ask_game()
    launch_chosen_game(game)

  // Demander à l'utilisateur le jeu qu'il souhaite résoudre
  def ask_game(): String =
    println("Quel jeu voulez-vous résoudre ?")
    println("- Un sudoku (S)")
    println("- Une carte colorée (C)")
    String(readLine())

  def launch_chosen_game(game: String): Unit =
    if (game == "S") {
      println("Welcome to the SUDOKU GAME !")
      sudoku()
    }
    else if(game == "C"){
      println("Welcome to the MAP GAME !")
      map_coloring()
    }
    else {
      println("Je n'ai pas compris votre choix.")
      val game: String = ask_game()
    }

  launch_game_solver()