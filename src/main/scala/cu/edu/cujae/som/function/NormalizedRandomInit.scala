package cu.edu.cujae.som.function

import cu.edu.cujae.som.data.VectorSet

import scala.util.Random

/**
 * Inicializa un set de vectores con valores aleatorios normalizados
 * entre los limites de las dimensiones del espacio de entrada
 */
protected [function] trait NormalizedRandomInit extends InitFn {
  /**
   * Aplica la funcion de inicializacion representada por esta interfaz
   *
   * @param vectors Vectores de peso a inicializar
   * @param vectorSet Espacio de entrada a utilizar en la inicializacion
   * @param seed Semilla para inicializacion aleatoria
   */
  override def apply (
                       vectors: Iterable[Array[Double]],
                       vectorSet: VectorSet, seed: Long
                     ): Unit = {
    // Obtiene los limites superior e inferior del conjunto de vectores
    val bounds = vectorSet.dimBounds
    val rand = new Random()
    rand.setSeed(seed)

    vectors.foreach(x => {
      // Genera un valor aleatorio para cada dimension
      for (i <- bounds.indices) {
        val dimMin = bounds(i)._1
        val dimMax = bounds(i)._2

        x.update(i, (rand.between(dimMin, dimMax) - dimMin) / (dimMax - dimMin) )
      }
    })
  }
}
