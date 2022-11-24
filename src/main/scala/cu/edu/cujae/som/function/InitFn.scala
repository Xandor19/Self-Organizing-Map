package cu.edu.cujae.som.function

import cu.edu.cujae.som.data.VectorSet

/**
 * Interfaz para representar una funcion de inicializacion generica
 */
trait InitFn {
  /**
   * Aplica la funcion de inicializacion generica representada por esta interfaz
   *
   * @param amount Cantidad de vectores de peso a inicializar
   * @param vectorSet Espacio de entrada a utilizar en la inicializacion
   * @param seed Semilla para inicializacion aleatoria
   */
  def apply(
             amount: Int,
             vectorSet: VectorSet, seed: Long
           ): Iterable[Array[Double]]
}


/**
 * Proporciona una funcion de factoria para funciones de inicializacion
 */
object InitFactory {
  /**
   * Proporciona una funcion de inicializacion especifica dado su identificador
   *
   * @param fn Identificador de la funcion de inicializacion
   * @return Funcion deseada o null si no existe el identificador
   */
  def apply (fn: String): InitFn = {
    fn match {
      case InitFns.RandomInit => new RandomInit {}
      case InitFns.NormalizedRandomInit => new NormalizedRandomInit {}
      case _ => null
    }
  }
}


/**
 * Identificadores para las funciones de inicializacion
 */
object InitFns {
  val RandomInit = "Random"
  val NormalizedRandomInit = "Normalized Random"
}