
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

        case Contraint.DiffVariables(x, y) =>
          !(are_domains_equal(domains(x), domains(y)))

        case Contraint.DiffConstant(x, c) =>
          !(are_domains_equal(domains(x), Domain(Set(c))))

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
          val newDomain: Domain[A] = set_domains_different(domains(x), domains(y))
          Map(x -> newDomain, y -> newDomain)
        
        case Contraint.DiffConstant(x, c) =>
          Map(x -> set_domains_different(domains(x), Domain(Set(c))))

        case _ => Map() // Contrainte inconnue


    def update_domains(newDomains: Map[Variable[A], Domain[A]]): CSP[A] =
      // Créer un nouvel objet CSP avec les domaines mis a jour
      copy(domains=domains++newDomains)


    def set_domains_equal(x: Domain[A], y: Domain[A]): Domain[A] =
      // Renvoie l'intersection des domaines x et y
      x.intersect(y)

    
    def set_domains_different(x: Domain[A], y: Domain[A]): Domain[A]
      // Renvoie 
      // Deux cas :
      // Les deux variables ne possèdent qu'une seule valeur => Il faut que les valeurs soient différentes
      // Au moins une des deux variables possède deux valeurs => Il y a forcément une solution

    def are_domains_equal(x: Domain[A], y: Domain[A]): Boolean =
      // True si les domaines sont égaux
      x.values.equals(y.values)


  //TESTS
  val dom1: Domain[Int] = Domain[Int](Set(1, 2, 3))
  //val dom2: Domain[Int] = Domain[Int](Set(1, 2, 3))
  //val dom3: Domain[Int] = Domain[Int](Set(1, 2, 3))

  val v1 = Variable[Int]("v1")
  val v2 = Variable[Int]("v2")
  val v3 = Variable[Int]("v3")

  val colorCsp: CSP[Int] =
    CSP(
      domains =
      // Each variable is bound to the same domain
        Map(
          v1 -> dom1,
          v2 -> dom1,
          v3 -> dom1
        ),
      constraints =
        Set(
          Constraint.EqualVariables(v1,v2),
          Constraint.EqualConstant(v2,2)
        )
    )
  print(colorCsp.solve)

