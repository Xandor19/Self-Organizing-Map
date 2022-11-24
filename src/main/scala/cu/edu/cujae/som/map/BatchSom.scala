package cu.edu.cujae.som.map

import cu.edu.cujae.som.data.VectorSet
import cu.edu.cujae.som.function.{DistanceFn, NeighborhoodFn}
import cu.edu.cujae.som.io.MapConfig

/**
 * Clase para representar un SOM con enfoque de entrenamiento en batch
 *
 * @param _lattice Grilla del mapa
 * @param _neighRadius Radio de vecindad inicial
 * @param _distanceFn Funcion de distancia a utilizar
 * @param _neighborhoodFn Funcion de vecindad a utilizar en el entrenamiento
 */
class BatchSom protected [map] (
                                 _lattice: Lattice,
                                 _neighRadius: Double,
                                 _distanceFn: DistanceFn,
                                 _neighborhoodFn: NeighborhoodFn
                               )
  extends Som (_lattice, _neighRadius, _distanceFn, _neighborhoodFn) {

  /**
   * Proceso de auto-organizacion a realizar mediante el enfoque en batch
   * por un numero especificado de iteraciones
   *
   * @param vectorSet Conjunto de vectores de entrada a emplear en el entrenamiento
   * @param mapConfig Parametros de configuracion del entrenamiento
   */
  override def organizeMap(
                            vectorSet: VectorSet,
                            mapConfig: MapConfig
                          ): Unit = {
    var decRadius = _neighRadius

    // Obtiene la cantidad de iteraciones
    val trainIters = mapConfig.trainIter

    for (t <- 0 until trainIters) {
      // Obtiene el iterador sobre los vectores de entrada
      val setIt = vectorSet.iterator

      // Agrupa cada vector de entrada en su BMU
      setIt.foreach(x => clusterInput(x))

      // Actualiza los vectores de pesos de cada neurona
      lattice.neurons.flatten.foreach(x => applySingleBatch(x, decRadius))

      // Actualiza el radio de vecindad
      decRadius = updateRadius(t, trainIters)

      // Reinicia el agrupamiento de la red
      lattice.neurons.flatten.foreach(x => x.clearRepresented())
    }

    // Asigna cada entrada a su BMU por ultima vez
    vectorSet.iterator.foreach(x => clusterInput(x))

    // Actualiza las metricas de la red
    somReady()
  }


  /**
   * Aplica el entrenamiento en batch a una unica neurona de la grilla
   * calculando la media de los vectores representados por la red ponderada
   * por la funcion de vecindad entre la neurona y la BMU de cada vector
   *
   * Emplea la formula
   *
   * wi(t + 1) = (sum from j (nj * hji(t) xmj)) / (sum from j (nj * hji(t)))
   *
   * Donde nj es la cantidad de vectores representados por una neurona j, hji es
   * la funcion de vecindad entre la neurona a actualizar i y la neurona j
   * y xmj es la media de los vectores representados por la neurona j
   *
   * @param current Neurona a entrenar
   * @param currentRadius Radio de vecindad actual
   */
  def applySingleBatch (
                         current: Neuron,
                         currentRadius: Double
                       ): Unit = {
    // Acumuladores de resultados parciales
    var accVector = new Array[Double](dimensionality)
    var accNeigh = 0D

    // Recorre el mapa
    lattice.neurons.flatten.foreach(bmu => {
      // Obtiene la cantidad de entradas representadas por la posible BMU actual
      val repAmm = bmu.representedInputs.size

      // La neurona es BMU para al menos una entrada
      if (repAmm > 0) {
        // Obtiene el valor de vecindad entre la neurona actual y la BMU
        val neighRank = _neighborhoodFn(bmu.xPos, bmu.yPos, current.xPos, current.yPos, currentRadius)

        // Obtiene la media de los vectores representados por la BMU
        accVector = accVector.zip(bmu.meanVector).map(x => x._1 + (neighRank * repAmm * x._2))
        accNeigh += repAmm * neighRank
      }
    })
    // Actualiza el vector de pesos por de la neurona con la media ponderada
    current.weightVector = accVector.map(x => x / accNeigh)
  }
}
