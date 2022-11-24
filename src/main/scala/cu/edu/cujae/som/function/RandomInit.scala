package cu.edu.cujae.som.function

import cu.edu.cujae.som.data.VectorSet

import scala.util.Random

/**
 * Inicializa un set de vectores con valores aleatorios entre los limites
 * de las dimensiones del espacio de entrada
 */
protected [function] trait RandomInit extends InitFn {
  /**
   * Aplica la funcion de inicializacion representada por esta interfaz
   *
   * @param amount Cantidad de vectores de peso a inicializar
   * @param vectorSet Espacio de entrada a utilizar en la inicializacion
   * @param seed Semilla para inicializacion aleatoria
   */
  override def apply (
                       amount: Int,
                       vectorSet: VectorSet, seed: Long
                     ): Iterable[Array[Double]] = {
    // Obtiene los limites superior e inferior del conjunto de vectores
    val bounds = vectorSet.dimBounds
    val rand = new Random()
    rand.setSeed(seed)

    // Crea la cantidad de vectores requerida
    for (_ <- 0 until amount ) yield
      // Genera un valor aleatorio para cada dimension
      bounds.map{ case (dimMin, dimMax) => rand.between(dimMin, dimMax) }
  }
}
