package mapColoring

import scala.io.StdIn.readLine
import constraintLibrary.*

def mapColoring(): Unit =
  val difficulty = askDifficultyColoring()
  launchChosenMap(difficulty)

def askDifficultyColoring(): String =
  println("Choisissez le niveau de la carte à résoudre :")
  println("- Facile - Australia states (AS)")
  println("- Medium - United states (US)")
  String(readLine())

def launchChosenMap(difficulty: String): Unit =

// We launch the correct map coloring difficulty
  if (difficulty == "AS") {
    println("Résolution de la carte Australie ...")

    val mapColoringCsp: CSP[String] = initializeMapColoringAustralia()
    solveMapColoringAustralia(mapColoringCsp: CSP[String])
  }
  else if (difficulty == "US"){
    println("Résolution de la carte United states")

    val mapColoringCsp: CSP[String] = initializeMapColoringUnitedStates()
    solveMapColoringUnitedStates(mapColoringCsp: CSP[String])
  }
  else {
    println("Je n'ai pas compris votre choix.")
    val difficulty: String = askDifficultyColoring()
    launchChosenMap(difficulty)
  }

def initializeMapColoringAustralia(): CSP[String] =

  // Possible colors
  val colorsDomain: Domain[String] = Domain[String](Set("Red", "Green", "Blue"))

  // Here we declare all the Australia states
  val westernAustralia = Variable[String]("Western Australia")
  val northernTerritory = Variable[String]("Northern Territory")
  val southAustralia = Variable[String]("South Australia")
  val queensland = Variable[String]("Queensland")
  val newSouthWales = Variable[String]("New South Wales")
  val victoria = Variable[String]("Victoria")
  val tasmania = Variable[String]("Tasmania")

  val mapColoringCsp: CSP[String] =
    CSP(
      domains =
      // Each variable is bound to the same domain
        Map(
          westernAustralia -> colorsDomain,
          northernTerritory -> colorsDomain,
          southAustralia -> colorsDomain,
          queensland -> colorsDomain,
          newSouthWales -> colorsDomain,
          victoria -> colorsDomain,
          tasmania -> colorsDomain,
        ),
      constraints =
        List(
          // Constraints we can fix
          Constraint.EqualConstant(southAustralia, "Blue"),
          Constraint.EqualConstant(queensland, "Red"),
          Constraint.EqualVariables(tasmania, queensland),

          // Constraints to solve the map coloring
          Constraint.DiffVariables(westernAustralia, northernTerritory),
          Constraint.DiffVariables(westernAustralia, southAustralia),

          Constraint.DiffVariables(northernTerritory, southAustralia),
          Constraint.DiffVariables(northernTerritory, queensland),

          Constraint.DiffVariables(queensland, southAustralia),
          Constraint.DiffVariables(queensland, newSouthWales),

          Constraint.DiffVariables(newSouthWales, southAustralia),
          Constraint.DiffVariables(newSouthWales, victoria),

          Constraint.DiffVariables(victoria, southAustralia),
        )
    )
  mapColoringCsp


def initializeMapColoringUnitedStates(): CSP[String] =

  // Possible colors
  val colorsDomain: Domain[String] = Domain[String](Set("Red", "Green", "Blue", "Yellow"))

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
  val newMexico = Variable[String]("New Mexico")
  val northDakota = Variable[String]("North Dakota")
  val southDakota = Variable[String]("South Dakota")
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
  val southCarolina = Variable[String]("South Carolina")
  val northCarolina = Variable[String]("North Carolina")
  val virginia = Variable[String]("Virginia")
  val wescVirginia = Variable[String]("Wesc Virginia")
  val maryland = Variable[String]("Maryland")
  val delaware = Variable[String]("Delaware")
  val newJersey = Variable[String]("New Jersey")
  val pennsylvania = Variable[String]("Pennsylvania")
  val connecticut = Variable[String]("Connecticut")
  val rhodeIsland = Variable[String]("Rhode Island")
  val massachusetts = Variable[String]("Massachusetts")
  val newYork = Variable[String]("New York")
  val vermont = Variable[String]("Vermont")
  val newHampshire = Variable[String]("New Hampshire")
  val maine = Variable[String]("Maine")

  val mapColoringCsp: CSP[String] =
    CSP(
      domains =
      // Each variable is bound to the same domain
        Map(
          washington -> colorsDomain,
          oregon -> colorsDomain,
          california -> colorsDomain,
          nevada -> colorsDomain,
          idaho -> colorsDomain,
          montana -> colorsDomain,
          wyoming -> colorsDomain,
          utah -> colorsDomain,
          colorado -> colorsDomain,
          arizona -> colorsDomain,
          newMexico -> colorsDomain,
          northDakota -> colorsDomain,
          southDakota -> colorsDomain,
          nebraska -> colorsDomain,
          kansas -> colorsDomain,
          oklahoma -> colorsDomain,
          texas -> colorsDomain,
          minnesota -> colorsDomain,
          iowa -> colorsDomain,
          missouri -> colorsDomain,
          arkansas -> colorsDomain,
          louisiana -> colorsDomain,
          wisconsia -> colorsDomain,
          michigan -> colorsDomain,
          illinois -> colorsDomain,
          indiana -> colorsDomain,
          ohio -> colorsDomain,
          kentucky -> colorsDomain,
          tennessee -> colorsDomain,
          mississippi -> colorsDomain,
          alabama -> colorsDomain,
          georgia -> colorsDomain,
          florida -> colorsDomain,
          southCarolina -> colorsDomain,
          northCarolina -> colorsDomain,
          virginia -> colorsDomain,
          wescVirginia -> colorsDomain,
          maryland -> colorsDomain,
          delaware -> colorsDomain,
          newJersey -> colorsDomain,
          pennsylvania -> colorsDomain,
          connecticut -> colorsDomain,
          rhodeIsland -> colorsDomain,
          massachusetts -> colorsDomain,
          newYork -> colorsDomain,
          vermont -> colorsDomain,
          newHampshire -> colorsDomain,
          maine -> colorsDomain,
        ),
      constraints =
        List(
          // Constraints we can fix manually to avoid multiple solutions
          Constraint.EqualConstant(arizona, "Green"),
          Constraint.EqualConstant(kansas, "Red"),
          Constraint.EqualVariables(arizona, ohio),
          Constraint.EqualConstant(oregon, "Blue"),
          Constraint.EqualConstant(southDakota, "Yellow"),
          Constraint.EqualConstant(newYork, "Blue"),
          Constraint.EqualConstant(montana, "Green"),
          Constraint.EqualConstant(arkansas, "Green"),
          Constraint.EqualConstant(georgia, "Yellow"),
          Constraint.EqualConstant(delaware, "Blue"),
          Constraint.EqualConstant(wisconsia, "Green"),
          Constraint.EqualConstant(northCarolina, "Red"),
          Constraint.EqualConstant(rhodeIsland, "Red"),
          Constraint.EqualConstant(utah, "Blue"),
          Constraint.EqualConstant(newJersey, "Yellow"),
          Constraint.EqualConstant(newHampshire, "Yellow"),
          Constraint.EqualConstant(alabama, "Green"),
          Constraint.EqualConstant(illinois, "Blue"),
          Constraint.EqualConstant(michigan, "Blue"),
          Constraint.EqualConstant(wescVirginia, "Blue"),
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
          Constraint.DiffVariables(utah, newMexico),

          // Arizona
          Constraint.DiffVariables(arizona, colorado),
          Constraint.DiffVariables(arizona, newMexico),

          // Montana
          Constraint.DiffVariables(montana, wyoming),
          Constraint.DiffVariables(montana, northDakota),
          Constraint.DiffVariables(montana, southDakota),

          // Wyoming
          Constraint.DiffVariables(wyoming, colorado),
          Constraint.DiffVariables(wyoming, southDakota),
          Constraint.DiffVariables(wyoming, nebraska),

          // Colorado
          Constraint.DiffVariables(colorado, newMexico),
          Constraint.DiffVariables(colorado, nebraska),
          Constraint.DiffVariables(colorado, kansas),
          Constraint.DiffVariables(colorado, oklahoma),

          // New Mexico
          Constraint.DiffVariables(newMexico, oklahoma),
          Constraint.DiffVariables(newMexico, texas),

          // North Dakota
          Constraint.DiffVariables(northDakota, southDakota),
          Constraint.DiffVariables(northDakota, minnesota),

          // South Dakota
          Constraint.DiffVariables(southDakota, nebraska),
          Constraint.DiffVariables(southDakota, minnesota),
          Constraint.DiffVariables(southDakota, iowa),

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
          Constraint.DiffVariables(ohio, wescVirginia),
          Constraint.DiffVariables(ohio, kentucky),

          // Kentucky
          Constraint.DiffVariables(kentucky, wescVirginia),
          Constraint.DiffVariables(kentucky, virginia),
          Constraint.DiffVariables(kentucky, tennessee),

          // Tennessee
          Constraint.DiffVariables(tennessee, virginia),
          Constraint.DiffVariables(tennessee, northCarolina),
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
          Constraint.DiffVariables(georgia, southCarolina),

          // South Carolina
          Constraint.DiffVariables(southCarolina, northCarolina),

          // North Carolina
          Constraint.DiffVariables(northCarolina, virginia),

          // Virginia
          Constraint.DiffVariables(virginia, wescVirginia),
          Constraint.DiffVariables(virginia, maryland),

          // Wesc Virginia
          Constraint.DiffVariables(wescVirginia, maryland),
          Constraint.DiffVariables(wescVirginia, pennsylvania),

          // Maryland
          Constraint.DiffVariables(maryland, pennsylvania),
          Constraint.DiffVariables(maryland, delaware),

          // Delaware
          Constraint.DiffVariables(delaware, newJersey),
          Constraint.DiffVariables(delaware, pennsylvania),

          // Pennsylvania
          Constraint.DiffVariables(pennsylvania, newJersey),
          Constraint.DiffVariables(pennsylvania, newYork),

          // New Jersey
          Constraint.DiffVariables(newJersey, newYork),

          // New York
          Constraint.DiffVariables(newYork, connecticut),
          Constraint.DiffVariables(newYork, massachusetts),
          Constraint.DiffVariables(newYork, vermont),

          // Connecticut
          Constraint.DiffVariables(connecticut, rhodeIsland),
          Constraint.DiffVariables(connecticut, massachusetts),

          // Rhode Island
          Constraint.DiffVariables(rhodeIsland, massachusetts),

          // Massachussets
          Constraint.DiffVariables(massachusetts, vermont),
          Constraint.DiffVariables(massachusetts, newHampshire),

          // Vermont
          Constraint.DiffVariables(vermont, newHampshire),

          // New Hampshire
          Constraint.DiffVariables(newHampshire, maine),
          
          // Maine
        )
    )
  mapColoringCsp



def solveMapColoringAustralia(mapColoringCsp: CSP[String]): Unit =
  val solvedMap: Map[Variable[String], Domain[String]] = mapColoringCsp.solve

  println()
  print("Western Australia => ")
  println(solvedMap(Variable("Western Australia")).values.head)

  print("Northern Territory => ")
  println(solvedMap(Variable("Northern Territory")).values.head)

  print("South Australia => ")
  println(solvedMap(Variable("South Australia")).values.head)

  print("Queensland => ")
  println(solvedMap(Variable("Queensland")).values.head)

  print("New South Wales => ")
  println(solvedMap(Variable("New South Wales")).values.head)

  print("Victoria => ")
  println(solvedMap(Variable("Victoria")).values.head)

  print("Tasmania => ")
  println(solvedMap(Variable("Tasmania")).values.head)

def solveMapColoringUnitedStates(mapColoringCsp: CSP[String]): Unit =
  val solvedMap: Map[Variable[String], Domain[String]] = mapColoringCsp.solve
  
  println()
  print("Washington => ")
  println(solvedMap(Variable("Washington")).values.head)

  print("Oregon => ")
  println(solvedMap(Variable("Oregon")).values.head)

  print("California => ")
  println(solvedMap(Variable("California")).values.head)

  print("Nevada => ")
  println(solvedMap(Variable("Nevada")).values.head)

  print("Idaho => ")
  println(solvedMap(Variable("Idaho")).values.head)
  
  print("Montana => ")
  println(solvedMap(Variable("Montana")).values.head)
  
  print("Wyoming => ")
  println(solvedMap(Variable("Wyoming")).values.head)

  print("Utah => ")
  println(solvedMap(Variable("Utah")).values.head)

  print("Colorado => ")
  println(solvedMap(Variable("Colorado")).values.head)

  print("Arizona => ")
  println(solvedMap(Variable("Arizona")).values.head)

  print("New Mexico => ")
  println(solvedMap(Variable("New Mexico")).values.head)

  print("North Dakota => ")
  println(solvedMap(Variable("North Dakota")).values.head)

  print("South Dakota => ")
  println(solvedMap(Variable("South Dakota")).values.head)

  print("Nebraska => ")
  println(solvedMap(Variable("Nebraska")).values.head)

  print("Kansas => ")
  println(solvedMap(Variable("Kansas")).values.head)

  print("Oklahoma => ")
  println(solvedMap(Variable("Oklahoma")).values.head)

  print("Texas => ")
  println(solvedMap(Variable("Texas")).values.head)

  print("Minnesota => ")
  println(solvedMap(Variable("Minnesota")).values.head)

  print("Iowa => ")
  println(solvedMap(Variable("Iowa")).values.head)

  print("Missouri => ")
  println(solvedMap(Variable("Missouri")).values.head)

  print("Arkansas => ")
  println(solvedMap(Variable("Arkansas")).values.head)

  print("Louisiana => ")
  println(solvedMap(Variable("Louisiana")).values.head)

  print("Wisconsia => ")
  println(solvedMap(Variable("Wisconsia")).values.head)

  print("Michigan => ")
  println(solvedMap(Variable("Michigan")).values.head)

  print("Illinois => ")
  println(solvedMap(Variable("Illinois")).values.head)

  print("Indiana => ")
  println(solvedMap(Variable("Indiana")).values.head)

  print("Ohio => ")
  println(solvedMap(Variable("Ohio")).values.head)

  print("Kentucky => ")
  println(solvedMap(Variable("Kentucky")).values.head)
  
  print("Tennessee => ")
  println(solvedMap(Variable("Tennessee")).values.head)

  print("Mississippi => ")
  println(solvedMap(Variable("Mississippi")).values.head)

  print("Alabama => ")
  println(solvedMap(Variable("Alabama")).values.head)

  print("Georgia => ")
  println(solvedMap(Variable("Georgia")).values.head)
  
  print("Florida => ")
  println(solvedMap(Variable("Florida")).values.head)

  print("South Carolina => ")
  println(solvedMap(Variable("South Carolina")).values.head)

  print("North Carolina => ")
  println(solvedMap(Variable("North Carolina")).values.head)

  print("Virginia => ")
  println(solvedMap(Variable("Virginia")).values.head)

  print("Wesc Virginia => ")
  println(solvedMap(Variable("Wesc Virginia")).values.head)
  
  print("Maryland => ")
  println(solvedMap(Variable("Maryland")).values.head)

  print("Delaware => ")
  println(solvedMap(Variable("Delaware")).values.head)

  print("New Jersey => ")
  println(solvedMap(Variable("New Jersey")).values.head)

  print("Pennsylvania => ")
  println(solvedMap(Variable("Pennsylvania")).values.head)

  print("Connecticut => ")
  println(solvedMap(Variable("Connecticut")).values.head)

  print("Rhode Island => ")
  println(solvedMap(Variable("Rhode Island")).values.head)

  print("Massachusetts => ")
  println(solvedMap(Variable("Massachusetts")).values.head)

  print("New York => ")
  println(solvedMap(Variable("New York")).values.head)

  print("Vermont => ")
  println(solvedMap(Variable("Vermont")).values.head)

  print("New Hampshire => ")
  println(solvedMap(Variable("New Hampshire")).values.head)

  print("Maine => ")
  println(solvedMap(Variable("Maine")).values.head)
  