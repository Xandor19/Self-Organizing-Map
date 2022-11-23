package cu.edu.cujae.som.function

/**
 * Interfaz para representar una funcion de vecindad mediante la funcion gaussiana:
 *
 * exp(squaredDistance(BMU, neighbor) / 2 * currentRadius**2
 */
trait GaussianNeighborhood extends NeighborhoodFn {
  /**
   * Aplica la funcion de vecindad representada por esta interfaz
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
            ): Double = {
    // Obtiene la distancia en la grilla entre la BMU y la neurona actual
    val distance = (Array(bmuX, bmuY) zip Array(neighX, neighY))
                    .map(x => math.pow(x._1 - x._2, 2)).sum

    // Se esta comparando la BMU con ella misma
    if (distance == 0) 1
    else {
      // Se aplica la funcion de reduccion sobre la neurona
      val value = math.exp( (-distance) / (2 * math.pow(neighRadius, 2)))

      if (value.isNaN) 0
      else value
    }
  }
}
