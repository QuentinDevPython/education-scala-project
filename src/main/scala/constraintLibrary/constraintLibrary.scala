package constraintLibrary

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


case class CSP[A](domains: Map[Variable[A], Domain[A]], constraints: List[Constraint[A]]):
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
        are_domains_equal(domains(x), domains(y))

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
    copy(domains = domains ++ newDomains)


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
        x.flatMap(a => map_domains_combinations(tail).map(e => a :: e)).filter(l => l.toSet.size == l.size)
  //res = res.filter(l => l.toSet.size == l.size)
  //res

  def set_all_domains_diff(domains_list: List[Domain[A]]): List[Domain[A]] =
    val all_combinations = map_domains_combinations(domains_list.map(d => d.values.toList))
    //val combinations_all_different = all_combinations.filter(l => l.toSet.size == l.size)
    all_combinations.transpose.map(l => Domain(l.toSet))