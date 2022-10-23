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
  def toSingleton: Domain[A] = Domain(Set(values.head))

  def length: Int = values.size

case class Variable[A](name: String)

enum Constraint[A]:
  case EqualVariables(x: Variable[A], y: Variable[A])
  case EqualConstant(x: Variable[A], c: A)
  case DiffVariables(x: Variable[A], y: Variable[A])
  case DiffConstant(x: Variable[A], c: A)
  case AllDiff(variables: List[Variable[A]])


case class CSP[A](domains: Map[Variable[A], Domain[A]], constraints: List[Constraint[A]]):
  object cacheAllDiff { // Pour ne pas avoir à calculer deux fois setAllDomainsDiff
    var cache: List[Domain[A]] = null
  }

  def solve: Map[Variable[A], Domain[A]] =
    //println(s"Contraintes restantes : ${constraints.size}")
    val constraint: Option[Constraint[A]] = constraints.find(c => !isSatisfied(c)) // Cherche la première contrainte non satisfaite

    constraint match
      case None => // Si toutes les contraintes sont satisfaites
        val varToReduce = domains.find(!_._2.isSingleton)
        varToReduce match
          case Some(v) =>
            if v._2.isEmpty then Map() // Si le domaine est vide on ne peut pas résoudre les contraintes
            // Si il reste des domaines à réduire on fixe une valeur et on résout
            else
              val newCSP = updateDomains(Map(v._1 -> v._2.toSingleton), constraints)
              newCSP.solve
          case None => domains
      case Some(x) => // Si une contrainte non satisfaite a été trouvée
        // On créer un nouveau CSP qu'on résout (récursivement)
        val newMap = applyConstraint(x)
        if (newMap.forall(_._2.isSingleton)) //Contrainte résolue : on l'enleve
          val newConstraints = constraints.diff(List(x))
          val newCSP = updateDomains(newMap, newConstraints)
          newCSP.solve
        else
          val newCSP = updateDomains(newMap, constraints)
          newCSP.solve

  def isSatisfied(constraint: Constraint[A]): Boolean =
  // Vérifie si la contrainte est satisfaite ou non
    constraint match
      case Constraint.EqualVariables(x, y) =>
        areDomainsEqual(domains(x), domains(y))

      case Constraint.EqualConstant(x, c) =>
        areDomainsEqual(domains(x), Domain(Set(c)))

      case Constraint.DiffVariables(x, y) =>
        areDomainsDifferent(domains(x), domains(y))

      case Constraint.DiffConstant(x, c) =>
        areDomainsDifferent(domains(x), Domain(Set(c)))

      case Constraint.AllDiff(variables) =>
        val oldDomains: List[Domain[A]] = variables.map(v => domains(v))
        val newDomains: List[Domain[A]] = setAllDomainsDiff(variables.map(v => domains(v)))
        if (newDomains == oldDomains) true //Contrainte satisfaite
        else
          cacheAllDiff.cache = newDomains // Contrainte non satisfaite -> on stock newDomains
          false

      case _ => true

  def applyConstraint(constraint: Constraint[A]): Map[Variable[A], Domain[A]] =
  // Applique la contrainte aux variables concernées
  // Renvoie une Map qui permet de mettre à jour les "domains"
    constraint match
      case Constraint.EqualVariables(x, y) =>
        val newDomain: Domain[A] = setDomainsEqual(domains(x), domains(y))
        Map(x -> newDomain, y -> newDomain)

      case Constraint.EqualConstant(x, c) =>
        Map(x -> setDomainsEqual(domains(x), Domain(Set(c))))

      case Constraint.DiffVariables(x, y) =>
        val newDomain: List[Domain[A]] = setDomainsDifferent(domains(x), domains(y))
        Map(x -> newDomain.head, y -> newDomain.last)

      case Constraint.DiffConstant(x, c) =>
        Map(x -> setDomainsDifferentConstant(domains(x), Domain(Set(c))))

      case Constraint.AllDiff(variables: List[Variable[A]]) =>
        val newDomains = cacheAllDiff.cache
        variables.zip(newDomains).toMap

      case _ => Map() // Contrainte inconnue


  def updateDomains(newDomains: Map[Variable[A], Domain[A]], newConstraints: List[Constraint[A]]): CSP[A] =
  // Créer un nouvel objet CSP avec les domaines mis a jour
    copy(domains = domains ++ newDomains, constraints = newConstraints)


  def setDomainsEqual(x: Domain[A], y: Domain[A]): Domain[A] =
  // Renvoie l'intersection des domaines x et y
    x.intersect(y)


  def setDomainsDifferent(x: Domain[A], y: Domain[A]): List[Domain[A]] =
  // Renvoie une liste des domaines x et y
    if (x.isSingleton) List(x, y.diff(x))

    else if (y.isSingleton) List(x.diff(y), y)

    else List(x, y)

  def setDomainsDifferentConstant(x: Domain[A], y: Domain[A]): Domain[A] =
    x.diff(y)

  def areDomainsEqual(x: Domain[A], y: Domain[A]): Boolean =
  // True si les domaines sont égaux
    x.values.equals(y.values)


  def areDomainsDifferent(x: Domain[A], y: Domain[A]): Boolean =
    if (x.isSingleton || y.isSingleton) x.intersect(y).isEmpty
    else true

  def mapDomainsCombinations(domainsList: List[List[A]]): List[List[A]] =
    domainsList match
      case Nil => List(Nil)
      case xs :: tail =>
        val combinationsTail = mapDomainsCombinations(tail)
        xs.flatMap(x =>
          combinationsTail.flatMap(c =>
            val nList = x::c
            if (nList.distinct.size == nList.size) Some(x::c)
            else None
          )
        )


  def setAllDomainsDiff(domainsList: List[Domain[A]]): List[Domain[A]] =
    val allCombinations = mapDomainsCombinations(domainsList.map(d => d.values.toList))
    allCombinations.transpose.map(l => Domain(l.toSet))