package cu.edu.cujae.som.function

/**
 * Interfaz para representar una funcion de vecindad generica
 */
trait NeighborhoodFn {
  /**
   * Aplica la funcion de vecindad generica representada por esta interfaz
   *
   * @param bmuX Coordenada X de la BMU
   * @param bmuY Coordenada Y de la BMU
   * @param neighX Coordenada X de la neurona vecina
   * @param neighY Coordenada Y de la neurona vecina
   * @param neighRadius Radio de vecindad actual
   * @return Valor de vecindad para la neurona con respecto a la BMU
   */
  def apply (
              bmuX: Float,
              bmuY: Float,
              neighX: Float,
              neighY: Float,
              neighRadius: Double
            ): Double
}


/**
 * Proporciona una funcion de factoria para funciones de vecindad
 */
object NeighborhoodFactory {
  /**
   * Proporciona una funcion de vecindad especifica dado su identificador
   *
   * @param fn Identificador de la funcion de inicializacion
   * @return Funcion deseada o null si no existe el identificador
   */
  def apply (fn: String): NeighborhoodFn = {
    fn match {
      case NeighboringFns.Gaussian => new GaussianNeighborhood {}
      case _ => null
    }
  }
}


/**
 * Identificadores para las funciones de vecindad
 */
object NeighboringFns {
  val Gaussian = "Gaussian"
}
