package mapColoring

import scala.io.StdIn.readLine
import constraintLibrary.*


def mapColoring(): Unit =
  val difficulty = askDifficultyColoring()
  launchChosenMap(difficulty)


def askDifficultyColoring(): String =
  println("Choisissez le niveau de la carte à résoudre :")
  println("- Facile - L'Australie (A)")
  println("- Medium - Les Etats-Unis (EU)")
  String(readLine())


def launchChosenMap(difficulty: String): Unit =

// We launch the correct map coloring difficulty
  if (difficulty == "A") {
    println("Résolution de la carte Australie ...")

    val listAustraliaStates: List[String] = List[String](
      "Western Australia", "Northern Territory", "South Australia",
      "Queensland", "New South Wales", "Victoria", "Tasmania"
    )

    val mapColoringCsp: CSP[String] = initializeMapColoringAustralia(listAustraliaStates: List[String])

    solveAndPrintMapColoring(listAustraliaStates: List[String], mapColoringCsp: CSP[String])
  }
  else if (difficulty == "EU"){
    println("Résolution de la carte Etats-Unis ...")

    val listUnitedStates: List[String] = List[String](
      "Washington", "Oregon", "California", "Nevada", "Idaho",
      "Montana", "Wyoming", "Utah", "Colorado", "Arizona",
      "New Mexico", "North Dakota", "South Dakota", "Nebraska",
      "Kansas", "Oklahoma", "Texas", "Minnesota", "Iowa",
      "Missouri", "Arkansas", "Louisiana", "Wisconsia",
      "Michigan", "Illinois", "Indiana", "Ohio", "Kentucky",
      "Tennessee", "Mississippi", "Alabama", "Georgia",
      "Florida", "South Carolina", "North Carolina",
      "Virginia", "Wesc Virginia", "Maryland", "Delaware",
      "New Jersey", "Pennsylvania", "Connecticut",
      "Rhode Island", "Massachusetts", "New York", "Vermont",
      "New Hampshire", "Maine"
    )

    val mapColoringCsp: CSP[String] = initializeMapColoringUnitedStates(listUnitedStates: List[String])

    solveAndPrintMapColoring(listUnitedStates: List[String], mapColoringCsp: CSP[String])
  }
  else {
    println("Je n'ai pas compris votre choix.")
    val difficulty: String = askDifficultyColoring()
    launchChosenMap(difficulty)
  }


def initializeMapColoringAustralia(listAustraliaStates: List[String]): CSP[String] =

  // Possible colors
  val colorsDomain: Domain[String] = Domain[String](Set("Red", "Green", "Blue"))

  var mapStateWithVariable: Map[String, Variable[String]] = Map[String, Variable[String]]()

  for (stateName <- listAustraliaStates){
    mapStateWithVariable = mapStateWithVariable + (stateName -> Variable[String](stateName))
  }

  var mapStateWithColorDomain:  Map[Variable[String], Domain[String]] = Map()

  for (stateVariable <- mapStateWithVariable.values){
    mapStateWithColorDomain = mapStateWithColorDomain + (stateVariable -> colorsDomain)
  }

  val mapColoringCsp: CSP[String] =
    CSP(
      domains = mapStateWithColorDomain,
      // Each variable is bound to the same domain
      constraints =
        List(
          // Constraints we can fix manually to avoid multiple solutions
          Constraint.EqualConstant(mapStateWithVariable("South Australia"), "Blue"),
          Constraint.EqualConstant(mapStateWithVariable("Queensland"), "Red"),
          Constraint.EqualVariables(mapStateWithVariable("Tasmania"), mapStateWithVariable("Queensland")),

          // Constraints to solve the map coloring
          Constraint.DiffVariables(mapStateWithVariable("Western Australia"), mapStateWithVariable("Northern Territory")),
          Constraint.DiffVariables(mapStateWithVariable("Western Australia"), mapStateWithVariable("South Australia")),

          Constraint.DiffVariables(mapStateWithVariable("Northern Territory"), mapStateWithVariable("South Australia")),
          Constraint.DiffVariables(mapStateWithVariable("Northern Territory"), mapStateWithVariable("Queensland")),

          Constraint.DiffVariables(mapStateWithVariable("Queensland"), mapStateWithVariable("South Australia")),
          Constraint.DiffVariables(mapStateWithVariable("Queensland"), mapStateWithVariable("New South Wales")),

          Constraint.DiffVariables(mapStateWithVariable("New South Wales"), mapStateWithVariable("South Australia")),
          Constraint.DiffVariables(mapStateWithVariable("New South Wales"), mapStateWithVariable("Victoria")),

          Constraint.DiffVariables(mapStateWithVariable("Victoria"), mapStateWithVariable("South Australia")),
        )
    )
  mapColoringCsp


def initializeMapColoringUnitedStates(listUnitedStates: List[String]): CSP[String] =

  // Possible colors
  val colorsDomain: Domain[String] = Domain[String](Set("Red", "Green", "Blue", "Yellow"))

  var mapStateWithVariable: Map[String, Variable[String]] = Map[String, Variable[String]]()

  for (stateName <- listUnitedStates){
    mapStateWithVariable = mapStateWithVariable + (stateName -> Variable[String](stateName))
  }

  var mapStateWithColorDomain:  Map[Variable[String], Domain[String]] = Map()

  for (stateVariable <- mapStateWithVariable.values){
    mapStateWithColorDomain = mapStateWithColorDomain + (stateVariable -> colorsDomain)
  }

  val mapColoringCsp: CSP[String] =
    CSP(
      domains = mapStateWithColorDomain,
    
      constraints =
        List(
          // Constraints we can fix manually to avoid multiple solutions
          // Green
          Constraint.EqualConstant(mapStateWithVariable("Arizona"), "Green"),
          Constraint.EqualVariables(mapStateWithVariable("Ohio"), mapStateWithVariable("Arizona")),
          Constraint.EqualVariables(mapStateWithVariable("Montana"), mapStateWithVariable("Arizona")),
          Constraint.EqualVariables(mapStateWithVariable("Arkansas"), mapStateWithVariable("Arizona")),
          Constraint.EqualVariables(mapStateWithVariable("Alabama"), mapStateWithVariable("Arizona")),
          Constraint.EqualVariables(mapStateWithVariable("Wisconsia"), mapStateWithVariable("Arizona")),
          Constraint.EqualVariables(mapStateWithVariable("Virginia"), mapStateWithVariable("Arizona")),
          // Red
          Constraint.EqualConstant(mapStateWithVariable("North Carolina"), "Red"),
          Constraint.EqualVariables(mapStateWithVariable("Rhode Island"), mapStateWithVariable("North Carolina")),
          Constraint.EqualVariables(mapStateWithVariable("Kansas"), mapStateWithVariable("North Carolina")),
          Constraint.EqualVariables(mapStateWithVariable("Louisiana"), mapStateWithVariable("North Carolina")),
          // Blue
          Constraint.EqualConstant(mapStateWithVariable("Oregon"), "Blue"),
          Constraint.EqualVariables(mapStateWithVariable("New York"), mapStateWithVariable("Oregon")),
          Constraint.EqualVariables(mapStateWithVariable("Delaware"), mapStateWithVariable("Oregon")),
          Constraint.EqualVariables(mapStateWithVariable("Utah"), mapStateWithVariable("Oregon")),
          Constraint.EqualVariables(mapStateWithVariable("Illinois"), mapStateWithVariable("Oregon")),
          Constraint.EqualVariables(mapStateWithVariable("Michigan"), mapStateWithVariable("Oregon")),
          Constraint.EqualVariables(mapStateWithVariable("Wesc Virginia"), mapStateWithVariable("Oregon")),
          // Yellow
          Constraint.EqualConstant(mapStateWithVariable("New Jersey"), "Yellow"),
          Constraint.EqualVariables(mapStateWithVariable("New Hampshire"), mapStateWithVariable("New Jersey")),
          Constraint.EqualVariables(mapStateWithVariable("Georgia"), mapStateWithVariable("New Jersey")),
          Constraint.EqualVariables(mapStateWithVariable("South Dakota"), mapStateWithVariable("New Jersey")),

          // Constraints to solve the map coloring

          // Washington
          Constraint.DiffVariables(mapStateWithVariable("Washington"), mapStateWithVariable("Oregon")),
          Constraint.DiffVariables(mapStateWithVariable("Washington"), mapStateWithVariable("Idaho")),
          // Oregon
          Constraint.DiffVariables(mapStateWithVariable("Oregon"), mapStateWithVariable("Idaho")),
          Constraint.DiffVariables(mapStateWithVariable("Oregon"), mapStateWithVariable("California")),
          Constraint.DiffVariables(mapStateWithVariable("Oregon"), mapStateWithVariable("Nevada")),
          // California
          Constraint.DiffVariables(mapStateWithVariable("California"), mapStateWithVariable("Nevada")),
          Constraint.DiffVariables(mapStateWithVariable("California"), mapStateWithVariable("Arizona")),
          // Oregon
          Constraint.DiffVariables(mapStateWithVariable("Oregon"), mapStateWithVariable("Nevada")),
          // Nevada
          Constraint.DiffVariables(mapStateWithVariable("Nevada"), mapStateWithVariable("Idaho")),
          Constraint.DiffVariables(mapStateWithVariable("Nevada"), mapStateWithVariable("Utah")),
          Constraint.DiffVariables(mapStateWithVariable("Nevada"), mapStateWithVariable("Arizona")),
          // Idaho
          Constraint.DiffVariables(mapStateWithVariable("Idaho"), mapStateWithVariable("Montana")),
          Constraint.DiffVariables(mapStateWithVariable("Idaho"), mapStateWithVariable("Wyoming")),
          Constraint.DiffVariables(mapStateWithVariable("Idaho"), mapStateWithVariable("Utah")),
          // Utah
          Constraint.DiffVariables(mapStateWithVariable("Utah"), mapStateWithVariable("Arizona")),
          Constraint.DiffVariables(mapStateWithVariable("Utah"), mapStateWithVariable("Wyoming")),
          Constraint.DiffVariables(mapStateWithVariable("Utah"), mapStateWithVariable("Colorado")),
          Constraint.DiffVariables(mapStateWithVariable("Utah"), mapStateWithVariable("New Mexico")),
          // Arizona
          Constraint.DiffVariables(mapStateWithVariable("Arizona"), mapStateWithVariable("Colorado")),
          Constraint.DiffVariables(mapStateWithVariable("Arizona"), mapStateWithVariable("New Mexico")),

          // Montana
          Constraint.DiffVariables(mapStateWithVariable("Montana"), mapStateWithVariable("Wyoming")),
          Constraint.DiffVariables(mapStateWithVariable("Montana"), mapStateWithVariable("North Dakota")),
          Constraint.DiffVariables(mapStateWithVariable("Montana"), mapStateWithVariable("South Dakota")),

          // Wyoming
          Constraint.DiffVariables(mapStateWithVariable("Wyoming"), mapStateWithVariable("Colorado")),
          Constraint.DiffVariables(mapStateWithVariable("Wyoming"), mapStateWithVariable("South Dakota")),
          Constraint.DiffVariables(mapStateWithVariable("Wyoming"), mapStateWithVariable("Nebraska")),

          // Colorado
          Constraint.DiffVariables(mapStateWithVariable("Colorado"), mapStateWithVariable("New Mexico")),
          Constraint.DiffVariables(mapStateWithVariable("Colorado"), mapStateWithVariable("Nebraska")),
          Constraint.DiffVariables(mapStateWithVariable("Colorado"), mapStateWithVariable("Kansas")),
          Constraint.DiffVariables(mapStateWithVariable("Colorado"), mapStateWithVariable("Oklahoma")),

          // New Mexico
          Constraint.DiffVariables(mapStateWithVariable("New Mexico"), mapStateWithVariable("Oklahoma")),
          Constraint.DiffVariables(mapStateWithVariable("New Mexico"), mapStateWithVariable("Texas")),

          // North Dakota
          Constraint.DiffVariables(mapStateWithVariable("North Dakota"), mapStateWithVariable("South Dakota")),
          Constraint.DiffVariables(mapStateWithVariable("North Dakota"), mapStateWithVariable("Minnesota")),

          // South Dakota
          Constraint.DiffVariables(mapStateWithVariable("South Dakota"), mapStateWithVariable("Nebraska")),
          Constraint.DiffVariables(mapStateWithVariable("South Dakota"), mapStateWithVariable("Minnesota")),
          Constraint.DiffVariables(mapStateWithVariable("South Dakota"), mapStateWithVariable("Iowa")),

          // Kansas
          Constraint.DiffVariables(mapStateWithVariable("Kansas"), mapStateWithVariable("Missouri")),
          Constraint.DiffVariables(mapStateWithVariable("Kansas"), mapStateWithVariable("Oklahoma")),

          // Oklahoma
          Constraint.DiffVariables(mapStateWithVariable("Oklahoma"), mapStateWithVariable("Missouri")),
          Constraint.DiffVariables(mapStateWithVariable("Oklahoma"), mapStateWithVariable("Arkansas")),
          Constraint.DiffVariables(mapStateWithVariable("Oklahoma"), mapStateWithVariable("Texas")),

          // Texas
          Constraint.DiffVariables(mapStateWithVariable("Texas"), mapStateWithVariable("Arkansas")),
          Constraint.DiffVariables(mapStateWithVariable("Texas"), mapStateWithVariable("Louisiana")),

          // Minnesota
          Constraint.DiffVariables(mapStateWithVariable("Minnesota"), mapStateWithVariable("Wisconsia")),
          Constraint.DiffVariables(mapStateWithVariable("Minnesota"), mapStateWithVariable("Iowa")),

          // Iowa
          Constraint.DiffVariables(mapStateWithVariable("Iowa"), mapStateWithVariable("Wisconsia")),
          Constraint.DiffVariables(mapStateWithVariable("Iowa"), mapStateWithVariable("Illinois")),
          Constraint.DiffVariables(mapStateWithVariable("Iowa"), mapStateWithVariable("Missouri")),

          // Missouri
          Constraint.DiffVariables(mapStateWithVariable("Missouri"), mapStateWithVariable("Illinois")),
          Constraint.DiffVariables(mapStateWithVariable("Missouri"), mapStateWithVariable("Kentucky")),
          Constraint.DiffVariables(mapStateWithVariable("Missouri"), mapStateWithVariable("Tennessee")),
          Constraint.DiffVariables(mapStateWithVariable("Missouri"), mapStateWithVariable("Arkansas")),

          // Arkansas
          Constraint.DiffVariables(mapStateWithVariable("Arkansas"), mapStateWithVariable("Tennessee")),
          Constraint.DiffVariables(mapStateWithVariable("Arkansas"), mapStateWithVariable("Mississippi")),
          Constraint.DiffVariables(mapStateWithVariable("Arkansas"), mapStateWithVariable("Louisiana")),

          // Louisiana
          Constraint.DiffVariables(mapStateWithVariable("Louisiana"), mapStateWithVariable("Mississippi")),

          // Wisconsia
          Constraint.DiffVariables(mapStateWithVariable("Wisconsia"), mapStateWithVariable("Michigan")),
          Constraint.DiffVariables(mapStateWithVariable("Wisconsia"), mapStateWithVariable("Illinois")),

          // Illinois
          Constraint.DiffVariables(mapStateWithVariable("Illinois"), mapStateWithVariable("Indiana")),
          Constraint.DiffVariables(mapStateWithVariable("Illinois"), mapStateWithVariable("Kentucky")),

          // Michigan
          Constraint.DiffVariables(mapStateWithVariable("Michigan"), mapStateWithVariable("Indiana")),
          Constraint.DiffVariables(mapStateWithVariable("Michigan"), mapStateWithVariable("Ohio")),

          // Indiana
          Constraint.DiffVariables(mapStateWithVariable("Indiana"), mapStateWithVariable("Ohio")),
          Constraint.DiffVariables(mapStateWithVariable("Indiana"), mapStateWithVariable("Kentucky")),

          // Ohio
          Constraint.DiffVariables(mapStateWithVariable("Ohio"), mapStateWithVariable("Pennsylvania")),
          Constraint.DiffVariables(mapStateWithVariable("Ohio"), mapStateWithVariable("Wesc Virginia")),
          Constraint.DiffVariables(mapStateWithVariable("Ohio"), mapStateWithVariable("Kentucky")),

          // Kentucky
          Constraint.DiffVariables(mapStateWithVariable("Kentucky"), mapStateWithVariable("Wesc Virginia")),
          Constraint.DiffVariables(mapStateWithVariable("Kentucky"), mapStateWithVariable("Virginia")),
          Constraint.DiffVariables(mapStateWithVariable("Kentucky"), mapStateWithVariable("Tennessee")),

          // Tennessee
          Constraint.DiffVariables(mapStateWithVariable("Tennessee"), mapStateWithVariable("Virginia")),
          Constraint.DiffVariables(mapStateWithVariable("Tennessee"), mapStateWithVariable("North Carolina")),
          Constraint.DiffVariables(mapStateWithVariable("Tennessee"), mapStateWithVariable("Georgia")),
          Constraint.DiffVariables(mapStateWithVariable("Tennessee"), mapStateWithVariable("Alabama")),
          Constraint.DiffVariables(mapStateWithVariable("Tennessee"), mapStateWithVariable("Mississippi")),

          // Mississippi
          Constraint.DiffVariables(mapStateWithVariable("Mississippi"), mapStateWithVariable("Alabama")),

          // Alabama
          Constraint.DiffVariables(mapStateWithVariable("Alabama"), mapStateWithVariable("Georgia")),
          Constraint.DiffVariables(mapStateWithVariable("Alabama"), mapStateWithVariable("Florida")),

          // Florida
          Constraint.DiffVariables(mapStateWithVariable("Florida"), mapStateWithVariable("Georgia")),

          // Georgia
          Constraint.DiffVariables(mapStateWithVariable("Georgia"), mapStateWithVariable("South Carolina")),

          // South Carolina
          Constraint.DiffVariables(mapStateWithVariable("South Carolina"), mapStateWithVariable("North Carolina")),

          // North Carolina
          Constraint.DiffVariables(mapStateWithVariable("North Carolina"), mapStateWithVariable("Virginia")),

          // Virginia
          Constraint.DiffVariables(mapStateWithVariable("Virginia"), mapStateWithVariable("Wesc Virginia")),
          Constraint.DiffVariables(mapStateWithVariable("Virginia"), mapStateWithVariable("Maryland")),

          // Wesc Virginia
          Constraint.DiffVariables(mapStateWithVariable("Wesc Virginia"), mapStateWithVariable("Maryland")),
          Constraint.DiffVariables(mapStateWithVariable("Wesc Virginia"), mapStateWithVariable("Pennsylvania")),

          // Maryland
          Constraint.DiffVariables(mapStateWithVariable("Maryland"), mapStateWithVariable("Pennsylvania")),
          Constraint.DiffVariables(mapStateWithVariable("Maryland"), mapStateWithVariable("Delaware")),

          // Delaware
          Constraint.DiffVariables(mapStateWithVariable("Delaware"), mapStateWithVariable("New Jersey")),
          Constraint.DiffVariables(mapStateWithVariable("Delaware"), mapStateWithVariable("Pennsylvania")),

          // Pennsylvania
          Constraint.DiffVariables(mapStateWithVariable("Pennsylvania"), mapStateWithVariable("New Jersey")),
          Constraint.DiffVariables(mapStateWithVariable("Pennsylvania"), mapStateWithVariable("New York")),

          // New Jersey
          Constraint.DiffVariables(mapStateWithVariable("New Jersey"), mapStateWithVariable("New York")),

          // New York
          Constraint.DiffVariables(mapStateWithVariable("New York"), mapStateWithVariable("Connecticut")),
          Constraint.DiffVariables(mapStateWithVariable("New York"), mapStateWithVariable("Massachusetts")),
          Constraint.DiffVariables(mapStateWithVariable("New York"), mapStateWithVariable("Vermont")),

          // Connecticut
          Constraint.DiffVariables(mapStateWithVariable("Connecticut"), mapStateWithVariable("Rhode Island")),
          Constraint.DiffVariables(mapStateWithVariable("Connecticut"), mapStateWithVariable("Massachusetts")),

          // Rhode Island
          Constraint.DiffVariables(mapStateWithVariable("Rhode Island"), mapStateWithVariable("Massachusetts")),

          // Massachussets
          Constraint.DiffVariables(mapStateWithVariable("Massachusetts"), mapStateWithVariable("Vermont")),
          Constraint.DiffVariables(mapStateWithVariable("Massachusetts"), mapStateWithVariable("New Hampshire")),

          // Vermont
          Constraint.DiffVariables(mapStateWithVariable("Vermont"), mapStateWithVariable("New Hampshire")),

          // New Hampshire
          Constraint.DiffVariables(mapStateWithVariable("New Hampshire"), mapStateWithVariable("Maine")),
          
          // Maine
        )
    )
  mapColoringCsp


def solveAndPrintMapColoring(listStates: List[String], mapColoringCsp: CSP[String]): Unit=

  val solvedMap: Map[Variable[String], Domain[String]] = mapColoringCsp.solve

  println()
  for (state <- listStates){
    print(s"$state => ")
    println(solvedMap(Variable(s"$state")).values.head)
  }