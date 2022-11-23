package cu.edu.cujae.som.function

/**
 * Interfaz para representar una funcion de distancia generica
 */
trait DistanceFn {
  /**
   * Aplica la funcion de distancia generica representada por esta interfaz
   * @param arr1 1er Vector
   * @param arr2 2do Vector
   * @return Valor de distancia
   */
  def apply (
              arr1: Array[Double],
              arr2: Array[Double]
            ): Double
}


/**
 * Proporciona una funcion de factoria para funciones de distancia
 */
object DistanceFactory {
  /**
   * Proporciona una funcion de distancia especifica dado su identificador
   *
   * @param fn Identificador de la funcion de inicializacion
   * @return Funcion deseada o null si no existe el identificador
   */
  def apply (fn: String): DistanceFn = {
    fn match {
      case DistanceFns.SimpleEuclidean => new EuclideanDistance {}
      case DistanceFns.SquaredEuclidean => new SquaredEuclideanDistance {}
      case _ => null
    }
  }
}


/**
 * Identificadores para las funciones de distancia
 */
object DistanceFns {
  val SimpleEuclidean = "Euclidean"
  val SquaredEuclidean = "Squared Euclidean"
}
