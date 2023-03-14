package andxor

import scala.compiletime.summonAll

opaque type AndXorInstances[TC[_], T <: Tuple] = Tuple.Map[T, TC]

object AndXorInstances {
  extension [TC[_], T <: Tuple](i: AndXorInstances[TC, T]) def instances: Tuple.Map[T, TC] = i

  @inline private[andxor] def apply[TC[_], T <: Tuple](t: Tuple.Map[T, TC]): AndXorInstances[TC, T] = t

  inline given tuple[TC[_], T <: Tuple]: AndXorInstances[TC, T] = summonAll[Tuple.Map[T, TC]]
}
