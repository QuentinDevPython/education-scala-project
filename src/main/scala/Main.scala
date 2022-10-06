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


