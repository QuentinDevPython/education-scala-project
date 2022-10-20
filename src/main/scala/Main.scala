@main
def main: Unit =

  case class Domain[A](values: Set[A]):
    def union(domain: Domain[A]): Domain[A] = Domain(values.union(domain.values))
    def union(value: A): Domain[A] = union(domain = Domain[A](Set(value)))

    def intersect(domain: Domain[A]): Domain[A] = Domain(values.intersect(domain.values))
    def intersect(value: A): Domain[A] = intersect(domain = Domain[A](Set(value)))

    def diff(domain: Domain[A]): Domain[A] = Domain(values.diff(domain.values))
    def diff(value: A): Domain[A] = diff(domain = Domain[A](Set(value)))

    def isEmpty: Boolean = values.isEmpty
    def isSingleton: Boolean = values.size == 1

    def length: Int = values.size

  case class Variable[A](name: String)

  enum Constraint[A]:
    case EqualVariables(x: Variable[A], y: Variable[A])
    case EqualConstant(x: Variable[A], c: A)
    case DiffVariables(x: Variable[A], y: Variable[A])
    case DiffConstant(x: Variable[A], c: A)
    case AllDiff(variables: List[Variable[A]])


  case class CSP[A](domains: Map[Variable[A], Domain[A]], constraints: Set[Constraint[A]]):
    def solve: Map[Variable[A], Domain[A]] =
      val constraint: Option[Constraint[A]] = constraints.find(c => !is_satisfied(c)) // Cherche la première contrainte non satisfaite
      constraint match
        case None => domains // Si elles le sont toutes on a trouvé notre Map
        case Some(x) => // Si une contrainte non satisfaite a été trouvée
          // On créer un nouveau CSP qu'on résout (récursivement)
          val newCSP: CSP[A] = update_domains(apply_constraint(x))
          newCSP.solve


    def is_satisfied(constraint: Constraint[A]): Boolean =
    // Vérifie si la contrainte est satisfaite ou non
      constraint match
        case Constraint.EqualVariables(x, y) =>
          are_domains_equal(domains(x),domains(y))

        case Constraint.EqualConstant(x, c) =>
          are_domains_equal(domains(x), Domain(Set(c)))

        case Constraint.DiffVariables(x, y) =>
          are_domains_different(domains(x), domains(y))

        case Constraint.DiffConstant(x, c) =>
          are_domains_different(domains(x), Domain(Set(c)))

        case Constraint.AllDiff(variables) =>
          val oldDomains: List[Domain[A]] = variables.map(v => domains(v))
          val newDomains: List[Domain[A]] = set_all_domains_diff(variables.map(v => domains(v)))
          newDomains == oldDomains

        case _ => true


    def apply_constraint(constraint: Constraint[A]): Map[Variable[A], Domain[A]] =
    // Applique la contrainte aux variables concernées
    // Renvoie une Map qui permet de mettre à jour les "domains"
      constraint match
        case Constraint.EqualVariables(x, y) =>
          val newDomain: Domain[A] = set_domains_equal(domains(x), domains(y))
          Map(x -> newDomain, y -> newDomain)

        case Constraint.EqualConstant(x, c) =>
          Map(x -> set_domains_equal(domains(x), Domain(Set(c))))

        case Constraint.DiffVariables(x, y) =>
          val newDomain: List[Domain[A]] = set_domains_different(domains(x), domains(y))
          Map(x -> newDomain.head, y -> newDomain.tail.head)

        case Constraint.DiffConstant(x, c) =>
          Map(x -> set_domains_different_constant(domains(x), Domain(Set(c))))

        case Constraint.AllDiff(variables: List[Variable[A]]) =>
          val newDomains: List[Domain[A]] = set_all_domains_diff(variables.map(v => domains(v)))
          variables.zip(newDomains).toMap

        case _ => Map() // Contrainte inconnue


    def update_domains(newDomains: Map[Variable[A], Domain[A]]): CSP[A] =
    // Créer un nouvel objet CSP avec les domaines mis a jour
      copy(domains=domains++newDomains)


    def set_domains_equal(x: Domain[A], y: Domain[A]): Domain[A] =
    // Renvoie l'intersection des domaines x et y
      x.intersect(y)


    def set_domains_different(x: Domain[A], y: Domain[A]): List[Domain[A]] =
    // Renvoie une liste des domaines x et y
      if (x.isSingleton) List(x, y.diff(x))

      else if (y.isSingleton) List(x.diff(y), y)

      else List(x, y)

    def set_domains_different_constant(x: Domain[A], y: Domain[A]): Domain[A] =
      x.diff(y)

    def are_domains_equal(x: Domain[A], y: Domain[A]): Boolean =
    // True si les domaines sont égaux
      x.values.equals(y.values)


    def are_domains_different(x: Domain[A], y: Domain[A]): Boolean =
      if (x.isSingleton || y.isSingleton) x.intersect(y).isEmpty
      else true

    def map_domains_combinations(domains_list: List[List[A]]): List[List[A]] =
      domains_list match
        case Nil => List(List())
        case x :: tail =>
          val l = map_domains_combinations(tail)
          x.flatMap(a => l.map(e => a :: e))

    def set_all_domains_diff(domains_list: List[Domain[A]]): List[Domain[A]] =
      val all_combinations = map_domains_combinations(domains_list.map(d => d.values.toList))
      val combinations_all_different = all_combinations.filter(l => l.toSet.size == l.size)
      val new_domains = combinations_all_different.transpose.map(l => Domain(l.toSet))
      new_domains




  //TESTS
  val dom1: Domain[Int] = Domain[Int](Set(1, 2, 3, 4))
  val dom2: Domain[Int] = Domain[Int](Set(2, 3))
  val dom3: Domain[Int] = Domain[Int](Set(2, 3))

  val v1 = Variable[Int]("v1")
  val v2 = Variable[Int]("v2")
  val v3 = Variable[Int]("v3")

  val colorCsp: CSP[Int] =
    CSP(
      domains =
      // Each variable is bound to the same domain
        Map(
          v1 -> dom1,
          v2 -> dom2,
          v3 -> dom3
        ),
      constraints =
        Set(
          //Constraint.EqualVariables(v1,v3),
          //Constraint.EqualConstant(v2,2),
          //Constraint.DiffVariables(v1, v2),
          //Constraint.DiffConstant(v1, 1),
          Constraint.AllDiff(List(v1, v2, v3)),
          Constraint.EqualConstant(v2, 2)
        )
    )
  println(colorCsp.solve)



  // Sudoku implementation

  // Possible values than can take a sudoku box
  val possibilities_domain: Domain[Int] = Domain[Int](Set(1, 2, 3, 4, 5, 6, 7, 8, 9))

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

  val sudokuCsp: CSP[Int] =
    CSP(
      domains =
      // Each variable is bound to the same domain
        Map(
          v11 -> possibilities_domain,
          v12 -> possibilities_domain,
          v13 -> possibilities_domain,
          v14 -> possibilities_domain,
          v15 -> possibilities_domain,
          v16 -> possibilities_domain,
          v17 -> possibilities_domain,
          v18 -> possibilities_domain,
          v19 -> possibilities_domain,

          v21 -> possibilities_domain,
          v22 -> possibilities_domain,
          v23 -> possibilities_domain,
          v24 -> possibilities_domain,
          v25 -> possibilities_domain,
          v26 -> possibilities_domain,
          v27 -> possibilities_domain,
          v28 -> possibilities_domain,
          v29 -> possibilities_domain,

          v31 -> possibilities_domain,
          v32 -> possibilities_domain,
          v33 -> possibilities_domain,
          v34 -> possibilities_domain,
          v35 -> possibilities_domain,
          v36 -> possibilities_domain,
          v37 -> possibilities_domain,
          v38 -> possibilities_domain,
          v39 -> possibilities_domain,

          v41 -> possibilities_domain,
          v42 -> possibilities_domain,
          v43 -> possibilities_domain,
          v44 -> possibilities_domain,
          v45 -> possibilities_domain,
          v46 -> possibilities_domain,
          v47 -> possibilities_domain,
          v48 -> possibilities_domain,
          v49 -> possibilities_domain,

          v51 -> possibilities_domain,
          v52 -> possibilities_domain,
          v53 -> possibilities_domain,
          v54 -> possibilities_domain,
          v55 -> possibilities_domain,
          v56 -> possibilities_domain,
          v57 -> possibilities_domain,
          v58 -> possibilities_domain,
          v59 -> possibilities_domain,

          v61 -> possibilities_domain,
          v62 -> possibilities_domain,
          v63 -> possibilities_domain,
          v64 -> possibilities_domain,
          v65 -> possibilities_domain,
          v66 -> possibilities_domain,
          v67 -> possibilities_domain,
          v68 -> possibilities_domain,
          v69 -> possibilities_domain,

          v71 -> possibilities_domain,
          v72 -> possibilities_domain,
          v73 -> possibilities_domain,
          v74 -> possibilities_domain,
          v75 -> possibilities_domain,
          v76 -> possibilities_domain,
          v77 -> possibilities_domain,
          v78 -> possibilities_domain,
          v79 -> possibilities_domain,

          v81 -> possibilities_domain,
          v82 -> possibilities_domain,
          v83 -> possibilities_domain,
          v84 -> possibilities_domain,
          v85 -> possibilities_domain,
          v86 -> possibilities_domain,
          v87 -> possibilities_domain,
          v88 -> possibilities_domain,
          v89 -> possibilities_domain,

          v91 -> possibilities_domain,
          v92 -> possibilities_domain,
          v93 -> possibilities_domain,
          v94 -> possibilities_domain,
          v95 -> possibilities_domain,
          v96 -> possibilities_domain,
          v97 -> possibilities_domain,
          v98 -> possibilities_domain,
          v99 -> possibilities_domain,
        ),
      constraints =
        Set(
          // Initilizing the sudoku grid

          // All the 1 values
          Constraint.EqualConstant(v28,1),
          Constraint.EqualVariables(v28,v35),
          Constraint.EqualVariables(v28,v47),
          Constraint.EqualVariables(v28,v74),

          // All the 2 values
          Constraint.EqualConstant(v15,2),
          Constraint.EqualVariables(v15,v57),
          Constraint.EqualVariables(v15,v64),
          Constraint.EqualVariables(v15,v78),
          Constraint.EqualVariables(v15,v86),
          Constraint.EqualVariables(v15,v93),

          // All the 3 values
          Constraint.EqualConstant(v38,3),
          Constraint.EqualVariables(v38,v63),
          Constraint.EqualVariables(v38,v72),
          Constraint.EqualVariables(v38,v95),

          // All the 4 values
          Constraint.EqualConstant(v36,4),
          Constraint.EqualVariables(v36,v52),
          Constraint.EqualVariables(v36,v65),
          Constraint.EqualVariables(v36,v81),

          // All the 5 values
          Constraint.EqualConstant(v12,5),
          Constraint.EqualVariables(v12,v29),
          Constraint.EqualVariables(v12,v34),
          Constraint.EqualVariables(v12,v45),
          Constraint.EqualVariables(v12,v53),

          // All the 6 values
          Constraint.EqualConstant(v46,6),

          // All the 7 values
          Constraint.EqualConstant(v42,7),
          Constraint.EqualVariables(v42,v76),
          Constraint.EqualVariables(v42,v98),

          // All the 8 values
          Constraint.EqualConstant(v68,8),
          Constraint.EqualVariables(v68,v75),
          Constraint.EqualVariables(v68,v82),

          // All the 9 values
          Constraint.EqualConstant(v17,9),
          Constraint.EqualVariables(v17,v24),
          Constraint.EqualVariables(v17,v32),
          Constraint.EqualVariables(v17,v58),

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
    )

  val solved_sudoku: Map[Variable[Int], Domain[Int]] = sudokuCsp.solve

  println("-----------------------------------------")

  // 1st line
  print("|| ")
  print(solved_sudoku(Variable("v11")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v12")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v13")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v14")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v15")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v16")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v17")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v18")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v19")).values.head)
  println(" || ")

  // 2nd line
  print("|| ")
  print(solved_sudoku(Variable("v21")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v22")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v23")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v24")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v25")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v26")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v27")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v28")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v29")).values.head)
  println(" || ")

  // 3rd line
  print("|| ")
  print(solved_sudoku(Variable("v31")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v32")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v33")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v34")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v35")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v36")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v37")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v38")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v39")).values.head)
  println(" || ")

  println()

  // 4th line
  print("|| ")
  print(solved_sudoku(Variable("v41")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v42")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v43")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v44")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v45")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v46")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v47")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v48")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v49")).values.head)
  println(" || ")

  // 5th line
  print("|| ")
  print(solved_sudoku(Variable("v51")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v52")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v53")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v54")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v55")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v56")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v57")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v58")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v59")).values.head)
  println(" || ")

  // 6th line
  print("|| ")
  print(solved_sudoku(Variable("v61")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v62")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v63")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v64")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v65")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v66")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v67")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v68")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v69")).values.head)
  println(" || ")

  println()

  // 7th line
  print("|| ")
  print(solved_sudoku(Variable("v71")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v72")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v73")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v74")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v75")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v76")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v77")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v78")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v79")).values.head)
  println(" || ")

  // 8th line
  print("|| ")
  print(solved_sudoku(Variable("v81")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v82")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v83")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v84")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v85")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v86")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v87")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v88")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v89")).values.head)
  println(" || ")

  // 9th line
  print("|| ")
  print(solved_sudoku(Variable("v91")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v92")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v93")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v94")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v95")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v96")).values.head)
  print(" || ")
  print(solved_sudoku(Variable("v97")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v98")).values.head)
  print(" | ")
  print(solved_sudoku(Variable("v99")).values.head)
  println(" || ")

  println("-----------------------------------------")