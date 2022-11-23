package cu.edu.cujae.som.function

/**
 * Obtiene la distancia euclideana simple de dos vectores de pesos
 */
protected [function] trait EuclideanDistance extends DistanceFn {
  /**
   * Aplica la funcion de distancia representada por esta interfaz
   * @param arr1 1er vector
   * @param arr2 2do vector
   * @return Valor de distancia
   */
  override def apply (
                       arr1: Array[Double],
                       arr2: Array[Double]
                     ): Double = {
    math.sqrt((arr1 zip arr2).map(x => math.pow(x._1 - x._2, 2)).sum)
  }
}
