val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Constraint solver",
    version := "0.1.0-SNAPSHOT",
    developers := List(
      Developer(
        id    = "maxime",
        name  = "Maxime Bourgain",
        email = "maxime.bourgain@edu.esiee.fr",
        url   = url("https://github.com/Maxime200")
      ),
      Developer(
        id    = "baptiste",
        name  = "Baptiste Bontoux",
        email = "baptiste.bontoux@edu.esiee.fr",
        url   = url("https://github.com/BaptisteBtx")
      ),
      Developer(
        id    = "quentin",
        name  = "Quentin Barthélémy",
        email = "quentin.barthelemy@edu.esiee.fr",
        url   = url("https://github.com/QuentinDevPython/")
      ),
      Developer(
        id    = "clement",
        name  = "Clément Boudou",
        email = "clement.boudou@edu.esiee.fr",
        url   = url("https://github.com/clementB94")
      )
    ),
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "0.7.29" % Test)
  )
