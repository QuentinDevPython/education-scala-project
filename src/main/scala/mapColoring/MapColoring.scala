package mapColoring

import scala.io.StdIn.readLine
import constraintLibrary.*

def map_coloring(): Unit =
  val difficulty = ask_difficulty_coloring()
  launch_chosen_map(difficulty)

def ask_difficulty_coloring(): String =
  println("Choisissez le niveau de la carte à résoudre :")
  println("- Facile - Australie states (AS)")
  String(readLine())

def launch_chosen_map(difficulty: String): Unit =

// We launch the correct map coloring difficulty
  if (difficulty == "AS") {
    println("Résolution de la carte Australie ...")

    val mapColoringCsp: CSP[String] = initialize_map_coloring_australia()
    solve_map_coloring(mapColoringCsp: CSP[String])
  }
  else {
    println("Je n'ai pas compris votre choix.")
    val difficulty: String = ask_difficulty_coloring()
  }

def initialize_map_coloring_australia(): CSP[String] =

  // Possible colors
  val colors_domain: Domain[String] = Domain[String](Set("Red", "Green", "Blue"))

  // Here we declare all the Australia states
  val western_australia = Variable[String]("Western Australia")
  val northern_territory = Variable[String]("Northern Territory")
  val south_australia = Variable[String]("South Australia")
  val queensland = Variable[String]("Queensland")
  val new_south_wales = Variable[String]("New South Wales")
  val victoria = Variable[String]("Victoria")
  val tasmania = Variable[String]("Tasmania")

  val mapColoringCsp: CSP[String] =
    CSP(
      domains =
      // Each variable is bound to the same domain
        Map(
          western_australia -> colors_domain,
          northern_territory -> colors_domain,
          south_australia -> colors_domain,
          queensland -> colors_domain,
          new_south_wales -> colors_domain,
          victoria -> colors_domain,
          tasmania -> colors_domain,
        ),
      constraints =
        List(
          // Constraints we can fix
          Constraint.EqualConstant(south_australia, "Blue"),
          Constraint.EqualConstant(queensland, "Red"),
          Constraint.EqualVariables(tasmania, queensland),

          // Constraints to solve the map coloring
          Constraint.DiffVariables(western_australia, northern_territory),
          Constraint.DiffVariables(western_australia, south_australia),

          Constraint.DiffVariables(northern_territory, south_australia),
          Constraint.DiffVariables(northern_territory, queensland),

          Constraint.DiffVariables(queensland, south_australia),
          Constraint.DiffVariables(queensland, new_south_wales),

          Constraint.DiffVariables(new_south_wales, south_australia),
          Constraint.DiffVariables(new_south_wales, victoria),

          Constraint.DiffVariables(victoria, south_australia),
        )
    )
  mapColoringCsp

def solve_map_coloring(mapColoringCsp: CSP[String]): Unit =
  val solved_map: Map[Variable[String], Domain[String]] = mapColoringCsp.solve

  println()
  print("Western Australia => ")
  println(solved_map(Variable("Western Australia")).values.head)

  print("Northern Territory => ")
  println(solved_map(Variable("Northern Territory")).values.head)

  print("South Australia => ")
  println(solved_map(Variable("South Australia")).values.head)

  print("Queensland => ")
  println(solved_map(Variable("Queensland")).values.head)

  print("New South Wales => ")
  println(solved_map(Variable("New South Wales")).values.head)

  print("Victoria => ")
  println(solved_map(Variable("Victoria")).values.head)

  print("Tasmania => ")
  println(solved_map(Variable("Tasmania")).values.head)