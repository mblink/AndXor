package andxor

opaque type AndXorInstances[TC[_], T <: Tuple] = Tuple.Map[T, TC]

object AndXorInstances {
  extension [TC[_], T <: Tuple](i: AndXorInstances[TC, T]) def instances: Tuple.Map[T, TC] = i

  @inline private[andxor] def apply[TC[_], T <: Tuple](t: Tuple.Map[T, TC]): AndXorInstances[TC, T] = t

  given emptyTuple[TC[_]]: AndXorInstances[TC, EmptyTuple] = EmptyTuple

  given tupleN[TC[_], H, T <: Tuple](using h: TC[H], t: AndXorInstances[TC, T]): AndXorInstances[TC, H *: T] = h *: t
}
