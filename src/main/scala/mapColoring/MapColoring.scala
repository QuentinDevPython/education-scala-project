package mapColoring

import scala.io.StdIn.readLine
import constraintLibrary.*

def map_coloring(): Unit =
  val difficulty = ask_difficulty_coloring()
  launch_chosen_map(difficulty)

def ask_difficulty_coloring(): String =
  println("Choisissez le niveau de la carte à résoudre :")
  println("- Facile - Australia states (AS)")
  println("- Medium - United states (US)")
  String(readLine())

def launch_chosen_map(difficulty: String): Unit =

// We launch the correct map coloring difficulty
  if (difficulty == "AS") {
    println("Résolution de la carte Australie ...")

    val mapColoringCsp: CSP[String] = initialize_map_coloring_australia()
    solve_map_coloring_australia(mapColoringCsp: CSP[String])
  }
  else if (difficulty == "US"){
    println("Résolution de la carte United states")

    val mapColoringCsp: CSP[String] = initialize_map_coloring_united_states()
    solve_map_coloring_united_states(mapColoringCsp: CSP[String])
  }
  else {
    println("Je n'ai pas compris votre choix.")
    val difficulty: String = ask_difficulty_coloring()
    launch_chosen_map(difficulty)
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


def initialize_map_coloring_united_states(): CSP[String] =

  // Possible colors
  val colors_domain: Domain[String] = Domain[String](Set("Red", "Green", "Blue", "Yellow"))

  // Here we declare all the United states
  val washington = Variable[String]("Washington")
  val oregon = Variable[String]("Oregon")
  val california = Variable[String]("California")
  val nevada = Variable[String]("Nevada")
  val idaho = Variable[String]("Idaho")
  val montana = Variable[String]("Montana")
  val wyoming = Variable[String]("Wyoming")
  val utah = Variable[String]("Utah")
  val colorado = Variable[String]("Colorado")
  val arizona = Variable[String]("Arizona")
  val new_mexico = Variable[String]("New Mexico")
  val north_dakota = Variable[String]("North Dakota")
  val south_dakota = Variable[String]("South Dakota")
  val nebraska = Variable[String]("Nebraska")
  val kansas = Variable[String]("Kansas")
  val oklahoma = Variable[String]("Oklahoma")
  val texas = Variable[String]("Texas")
  val minnesota = Variable[String]("Minnesota")
  val iowa = Variable[String]("Iowa")
  val missouri = Variable[String]("Missouri")
  val arkansas = Variable[String]("Arkansas")
  val louisiana = Variable[String]("Louisiana")
  val wisconsia = Variable[String]("Wisconsia")
  val michigan = Variable[String]("Michigan")
  val illinois = Variable[String]("Illinois")
  val indiana = Variable[String]("Indiana")
  val ohio = Variable[String]("Ohio")
  val kentucky = Variable[String]("Kentucky")
  val tennessee = Variable[String]("Tennessee")
  val mississippi = Variable[String]("Mississippi")
  val alabama = Variable[String]("Alabama")
  val georgia = Variable[String]("Georgia")
  val florida = Variable[String]("Florida")
  val south_carolina = Variable[String]("South Carolina")
  val north_carolina = Variable[String]("North Carolina")
  val virginia = Variable[String]("Virginia")
  val wesc_virginia = Variable[String]("Wesc Virginia")
  val maryland = Variable[String]("Maryland")
  val delaware = Variable[String]("Delaware")
  val new_jersey = Variable[String]("New Jersey")
  val pennsylvania = Variable[String]("Pennsylvania")
  val connecticut = Variable[String]("Connecticut")
  val rhode_island = Variable[String]("Rhode Island")
  val massachusetts = Variable[String]("Massachusetts")
  val new_york = Variable[String]("New York")
  val vermont = Variable[String]("Vermont")
  val new_hampshire = Variable[String]("New Hampshire")
  val maine = Variable[String]("Maine")

  val mapColoringCsp: CSP[String] =
    CSP(
      domains =
      // Each variable is bound to the same domain
        Map(
          washington -> colors_domain,
          oregon -> colors_domain,
          california -> colors_domain,
          nevada -> colors_domain,
          idaho -> colors_domain,
          montana -> colors_domain,
          wyoming -> colors_domain,
          utah -> colors_domain,
          colorado -> colors_domain,
          arizona -> colors_domain,
          new_mexico -> colors_domain,
          north_dakota -> colors_domain,
          south_dakota -> colors_domain,
          nebraska -> colors_domain,
          kansas -> colors_domain,
          oklahoma -> colors_domain,
          texas -> colors_domain,
          minnesota -> colors_domain,
          iowa -> colors_domain,
          missouri -> colors_domain,
          arkansas -> colors_domain,
          louisiana -> colors_domain,
          wisconsia -> colors_domain,
          michigan -> colors_domain,
          illinois -> colors_domain,
          indiana -> colors_domain,
          ohio -> colors_domain,
          kentucky -> colors_domain,
          tennessee -> colors_domain,
          mississippi -> colors_domain,
          alabama -> colors_domain,
          georgia -> colors_domain,
          florida -> colors_domain,
          south_carolina -> colors_domain,
          north_carolina -> colors_domain,
          virginia -> colors_domain,
          wesc_virginia -> colors_domain,
          maryland -> colors_domain,
          delaware -> colors_domain,
          new_jersey -> colors_domain,
          pennsylvania -> colors_domain,
          connecticut -> colors_domain,
          rhode_island -> colors_domain,
          massachusetts -> colors_domain,
          new_york -> colors_domain,
          vermont -> colors_domain,
          new_hampshire -> colors_domain,
          maine -> colors_domain,
        ),
      constraints =
        List(
          // Constraints we can fix manually to avoid multiple solutions
          Constraint.EqualConstant(arizona, "Green"),
          Constraint.EqualConstant(kansas, "Red"),
          Constraint.EqualVariables(arizona, ohio),
          Constraint.EqualConstant(oregon, "Blue"),
          Constraint.EqualConstant(south_dakota, "Yellow"),
          Constraint.EqualConstant(new_york, "Blue"),
          Constraint.EqualConstant(montana, "Green"),
          Constraint.EqualConstant(arkansas, "Green"),
          Constraint.EqualConstant(georgia, "Yellow"),
          Constraint.EqualConstant(delaware, "Blue"),
          Constraint.EqualConstant(wisconsia, "Green"),
          Constraint.EqualConstant(north_carolina, "Red"),
          Constraint.EqualConstant(rhode_island, "Red"),
          Constraint.EqualConstant(utah, "Blue"),
          Constraint.EqualConstant(new_jersey, "Yellow"),
          Constraint.EqualConstant(new_hampshire, "Yellow"),
          Constraint.EqualConstant(alabama, "Green"),
          Constraint.EqualConstant(illinois, "Blue"),
          Constraint.EqualConstant(michigan, "Blue"),
          Constraint.EqualConstant(wesc_virginia, "Blue"),
          Constraint.EqualConstant(virginia, "Green"),
          Constraint.EqualConstant(louisiana, "Red"),

          // Constraints to solve the map coloring

          // Washington
          Constraint.DiffVariables(washington, oregon),
          Constraint.DiffVariables(washington, idaho),

          // Oregon
          Constraint.DiffVariables(oregon, idaho),
          Constraint.DiffVariables(oregon, california),
          Constraint.DiffVariables(oregon, nevada),

          // California
          Constraint.DiffVariables(california, nevada),
          Constraint.DiffVariables(california, arizona),

          // Oregon
          Constraint.DiffVariables(oregon, nevada),

          // Nevada
          Constraint.DiffVariables(nevada, idaho),
          Constraint.DiffVariables(nevada, utah),
          Constraint.DiffVariables(nevada, arizona),

          // Idaho
          Constraint.DiffVariables(idaho, montana),
          Constraint.DiffVariables(idaho, wyoming),
          Constraint.DiffVariables(idaho, utah),

          // Utah
          Constraint.DiffVariables(utah, arizona),
          Constraint.DiffVariables(utah, wyoming),
          Constraint.DiffVariables(utah, colorado),
          Constraint.DiffVariables(utah, new_mexico),

          // Arizona
          Constraint.DiffVariables(arizona, colorado),
          Constraint.DiffVariables(arizona, new_mexico),

          // Montana
          Constraint.DiffVariables(montana, wyoming),
          Constraint.DiffVariables(montana, north_dakota),
          Constraint.DiffVariables(montana, south_dakota),

          // Wyoming
          Constraint.DiffVariables(wyoming, colorado),
          Constraint.DiffVariables(wyoming, south_dakota),
          Constraint.DiffVariables(wyoming, nebraska),

          // Colorado
          Constraint.DiffVariables(colorado, new_mexico),
          Constraint.DiffVariables(colorado, nebraska),
          Constraint.DiffVariables(colorado, kansas),
          Constraint.DiffVariables(colorado, oklahoma),

          // New Mexico
          Constraint.DiffVariables(new_mexico, oklahoma),
          Constraint.DiffVariables(new_mexico, texas),

          // North Dakota
          Constraint.DiffVariables(north_dakota, south_dakota),
          Constraint.DiffVariables(north_dakota, minnesota),

          // South Dakota
          Constraint.DiffVariables(south_dakota, nebraska),
          Constraint.DiffVariables(south_dakota, minnesota),
          Constraint.DiffVariables(south_dakota, iowa),

          // Kansas
          Constraint.DiffVariables(kansas, missouri),
          Constraint.DiffVariables(kansas, oklahoma),

          // Oklahoma
          Constraint.DiffVariables(oklahoma, missouri),
          Constraint.DiffVariables(oklahoma, arkansas),
          Constraint.DiffVariables(oklahoma, texas),

          // Texas
          Constraint.DiffVariables(texas, arkansas),
          Constraint.DiffVariables(texas, louisiana),

          // Minnesota
          Constraint.DiffVariables(minnesota, wisconsia),
          Constraint.DiffVariables(minnesota, iowa),

          // Iowa
          Constraint.DiffVariables(iowa, wisconsia),
          Constraint.DiffVariables(iowa, illinois),
          Constraint.DiffVariables(iowa, missouri),

          // Missouri
          Constraint.DiffVariables(missouri, illinois),
          Constraint.DiffVariables(missouri, kentucky),
          Constraint.DiffVariables(missouri, tennessee),
          Constraint.DiffVariables(missouri, arkansas),

          // Arkansas
          Constraint.DiffVariables(arkansas, tennessee),
          Constraint.DiffVariables(arkansas, mississippi),
          Constraint.DiffVariables(arkansas, louisiana),

          // Louisiana
          Constraint.DiffVariables(louisiana, mississippi),

          // Wisconsia
          Constraint.DiffVariables(wisconsia, michigan),
          Constraint.DiffVariables(wisconsia, illinois),

          // Illinois
          Constraint.DiffVariables(illinois, indiana),
          Constraint.DiffVariables(illinois, kentucky),

          // Michigan
          Constraint.DiffVariables(michigan, indiana),
          Constraint.DiffVariables(michigan, ohio),

          // Indiana
          Constraint.DiffVariables(indiana, ohio),
          Constraint.DiffVariables(indiana, kentucky),

          // Ohio
          Constraint.DiffVariables(ohio, pennsylvania),
          Constraint.DiffVariables(ohio, wesc_virginia),
          Constraint.DiffVariables(ohio, kentucky),

          // Kentucky
          Constraint.DiffVariables(kentucky, wesc_virginia),
          Constraint.DiffVariables(kentucky, virginia),
          Constraint.DiffVariables(kentucky, tennessee),

          // Tennessee
          Constraint.DiffVariables(tennessee, virginia),
          Constraint.DiffVariables(tennessee, north_carolina),
          Constraint.DiffVariables(tennessee, georgia),
          Constraint.DiffVariables(tennessee, alabama),
          Constraint.DiffVariables(tennessee, mississippi),

          // Mississippi
          Constraint.DiffVariables(mississippi, alabama),

          // Alabama
          Constraint.DiffVariables(alabama, georgia),
          Constraint.DiffVariables(alabama, florida),

          // Florida
          Constraint.DiffVariables(florida, georgia),

          // Georgia
          Constraint.DiffVariables(georgia, south_carolina),

          // South Carolina
          Constraint.DiffVariables(south_carolina, north_carolina),

          // North Carolina
          Constraint.DiffVariables(north_carolina, virginia),

          // Virginia
          Constraint.DiffVariables(virginia, wesc_virginia),
          Constraint.DiffVariables(virginia, maryland),

          // Wesc Virginia
          Constraint.DiffVariables(wesc_virginia, maryland),
          Constraint.DiffVariables(wesc_virginia, pennsylvania),

          // Maryland
          Constraint.DiffVariables(maryland, pennsylvania),
          Constraint.DiffVariables(maryland, delaware),

          // Delaware
          Constraint.DiffVariables(delaware, new_jersey),
          Constraint.DiffVariables(delaware, pennsylvania),

          // Pennsylvania
          Constraint.DiffVariables(pennsylvania, new_jersey),
          Constraint.DiffVariables(pennsylvania, new_york),

          // New Jersey
          Constraint.DiffVariables(new_jersey, new_york),

          // New York
          Constraint.DiffVariables(new_york, connecticut),
          Constraint.DiffVariables(new_york, massachusetts),
          Constraint.DiffVariables(new_york, vermont),

          // Connecticut
          Constraint.DiffVariables(connecticut, rhode_island),
          Constraint.DiffVariables(connecticut, massachusetts),

          // Rhode Island
          Constraint.DiffVariables(rhode_island, massachusetts),

          // Massachussets
          Constraint.DiffVariables(massachusetts, vermont),
          Constraint.DiffVariables(massachusetts, new_hampshire),

          // Vermont
          Constraint.DiffVariables(vermont, new_hampshire),

          // New Hampshire
          Constraint.DiffVariables(new_hampshire, maine),
          
          // Maine
        )
    )
  mapColoringCsp



def solve_map_coloring_australia(mapColoringCsp: CSP[String]): Unit =
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

def solve_map_coloring_united_states(mapColoringCsp: CSP[String]): Unit =
  val solved_map: Map[Variable[String], Domain[String]] = mapColoringCsp.solve
  
  println()
  print("Washington => ")
  println(solved_map(Variable("Washington")).values.head)

  print("Oregon => ")
  println(solved_map(Variable("Oregon")).values.head)

  print("California => ")
  println(solved_map(Variable("California")).values.head)

  print("Nevada => ")
  println(solved_map(Variable("Nevada")).values.head)

  print("Idaho => ")
  println(solved_map(Variable("Idaho")).values.head)
  
  print("Montana => ")
  println(solved_map(Variable("Montana")).values.head)
  
  print("Wyoming => ")
  println(solved_map(Variable("Wyoming")).values.head)

  print("Utah => ")
  println(solved_map(Variable("Utah")).values.head)

  print("Colorado => ")
  println(solved_map(Variable("Colorado")).values.head)

  print("Arizona => ")
  println(solved_map(Variable("Arizona")).values.head)

  print("New Mexico => ")
  println(solved_map(Variable("New Mexico")).values.head)

  print("North Dakota => ")
  println(solved_map(Variable("North Dakota")).values.head)

  print("South Dakota => ")
  println(solved_map(Variable("South Dakota")).values.head)

  print("Nebraska => ")
  println(solved_map(Variable("Nebraska")).values.head)

  print("Kansas => ")
  println(solved_map(Variable("Kansas")).values.head)

  print("Oklahoma => ")
  println(solved_map(Variable("Oklahoma")).values.head)

  print("Texas => ")
  println(solved_map(Variable("Texas")).values.head)

  print("Minnesota => ")
  println(solved_map(Variable("Minnesota")).values.head)

  print("Iowa => ")
  println(solved_map(Variable("Iowa")).values.head)

  print("Missouri => ")
  println(solved_map(Variable("Missouri")).values.head)

  print("Arkansas => ")
  println(solved_map(Variable("Arkansas")).values.head)

  print("Louisiana => ")
  println(solved_map(Variable("Louisiana")).values.head)

  print("Wisconsia => ")
  println(solved_map(Variable("Wisconsia")).values.head)

  print("Michigan => ")
  println(solved_map(Variable("Michigan")).values.head)

  print("Illinois => ")
  println(solved_map(Variable("Illinois")).values.head)

  print("Indiana => ")
  println(solved_map(Variable("Indiana")).values.head)

  print("Ohio => ")
  println(solved_map(Variable("Ohio")).values.head)

  print("Kentucky => ")
  println(solved_map(Variable("Kentucky")).values.head)
  
  print("Tennessee => ")
  println(solved_map(Variable("Tennessee")).values.head)

  print("Mississippi => ")
  println(solved_map(Variable("Mississippi")).values.head)

  print("Alabama => ")
  println(solved_map(Variable("Alabama")).values.head)

  print("Georgia => ")
  println(solved_map(Variable("Georgia")).values.head)
  
  print("Florida => ")
  println(solved_map(Variable("Florida")).values.head)

  print("South Carolina => ")
  println(solved_map(Variable("South Carolina")).values.head)

  print("North Carolina => ")
  println(solved_map(Variable("North Carolina")).values.head)

  print("Virginia => ")
  println(solved_map(Variable("Virginia")).values.head)

  print("Wesc Virginia => ")
  println(solved_map(Variable("Wesc Virginia")).values.head)
  
  print("Maryland => ")
  println(solved_map(Variable("Maryland")).values.head)

  print("Delaware => ")
  println(solved_map(Variable("Delaware")).values.head)

  print("New Jersey => ")
  println(solved_map(Variable("New Jersey")).values.head)

  print("Pennsylvania => ")
  println(solved_map(Variable("Pennsylvania")).values.head)

  print("Connecticut => ")
  println(solved_map(Variable("Connecticut")).values.head)

  print("Rhode Island => ")
  println(solved_map(Variable("Rhode Island")).values.head)

  print("Massachusetts => ")
  println(solved_map(Variable("Massachusetts")).values.head)

  print("New York => ")
  println(solved_map(Variable("New York")).values.head)

  print("Vermont => ")
  println(solved_map(Variable("Vermont")).values.head)

  print("New Hampshire => ")
  println(solved_map(Variable("New Hampshire")).values.head)

  print("Maine => ")
  println(solved_map(Variable("Maine")).values.head)
  