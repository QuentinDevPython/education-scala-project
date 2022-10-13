@main def hello: Unit = 
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

def intersectionTest[A](aList: List[A], bList: List[A]): Option[A] = (aList.last, bList.last) match {
  case (a, b) if (a != b) => None
  case (_, _) if (aList.size > 1 && bList.size > 1 && aList(aList.size - 2) != bList(bList.size - 2)) => Some(aList.last)
  case (a, b) if (aList.size == 1 || bList.size == 1) && a == b => Some(a)
  case (_, _) => intersectionTest(aList.dropRight(1), bList.dropRight(1))
}

case class Domain[A](values: Set[A]):
  def union(domain: Domain[A]): Domain[A] =
    Domain(values.union(domain.values))

  def unionValue(value: A): Domain[A] =
    union(domain = Domain[A](Set(value)))

  def intersect(domain: Domain[A]): Domain[A] =
    Domain(values.intersect(domain.values))

  def intersectValue(value: A): Domain[A] =
    intersect(domain = Domain[A](Set(value)))

  def diff(domain: Domain[A]): Domain[A] =
    Domain(values.diff(domain.values))

  def diffValue(value: A): Domain[A] =
    diff(domain = Domain[A](Set(value)))

  def isEmpty: Boolean =
    values.isEmpty

  def isSingleton: Boolean =
    values.size == 1

// A variable is bound to a type and comes with a name
case class Variable[A](name: String)

// This enum type represents different types of constraints
enum Constraint[A]:
  case EqualVariables(x: Variable[A], y: Variable[A])
  case EqualConstant(x: Variable[A], c: A)
  case DiffVariables(x: Variable[A], y: Variable[A])
  case DiffConstant(x: Variable[A], c: A)
  case AllDiff(variables: List[Variable[A]])

// Represent a Constraint Satisfaction Problem (CSP)
// ie. associations between variables and domains,
//     and a set of constraints on those variables
case class CSP[A](domains: Map[Variable[A], Domain[A]], constraints: Set[Constraint[A]]):
  def solve: Map[Variable[A], Domain[A]] = ???