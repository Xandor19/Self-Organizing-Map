package cu.edu.cujae.som.map

import cu.edu.cujae.som.data.VectorSet
import cu.edu.cujae.som.function.{DistanceFn, NeighborhoodFn}
import cu.edu.cujae.som.io.MapConfig

/**
 * Clase para representar un SOM con enfoque de entrenamiento on-line
 *
 * @param _lattice Grilla del mapa
 * @param _learningFactor Factor de aprendizaje para entrenamiento
 * @param _tuningFactor Factor de aprendizaje para refinamiento
 * @param _neighRadius  Radio de vecindad inicial
 * @param distanceFn Funcion de distancia a utilizar
 * @param neighborhoodFn Funcion de vecindad a utilizar en el entrenamiento
 */
class OnlineSom protected [map] (
                                  _lattice: Lattice,
                                  private val _learningFactor: Double,
                                  private val _tuningFactor: Double,
                                  _neighRadius: Double,
                                  distanceFn: DistanceFn,
                                  neighborhoodFn: NeighborhoodFn
                                )
  extends Som (_lattice, _neighRadius, distanceFn, neighborhoodFn) {

  /**
   * Proceso de auto-organizacion a realizar mediante el enfoque on-line
   * por un numero especificado de iteraciones para cada vase
   *
   * @param vectorSet Conjunto de vectores de entrada a emplear en el entrenamiento
   * @param mapConfig Parametros de configuracion del entrenamiento
   */
  def organizeMap (
                    vectorSet: VectorSet,
                    mapConfig: MapConfig
                  ): Unit = {
    // Fase de entrenamiento de la red
    roughTraining(vectorSet, mapConfig.trainIter)

    // Ajusta el valor de vecindad para solo las vecinas inmediatas de las neuronas
    val fixedNeigh = neighborhoodFn(1, 1, 1, 0, 1)

    // Establece el factor de refinamiento para las neuronas
    lattice.neurons.flatten.foreach(x => x.tuningRate = tuningFactor)

    // Fase de refinamiento de la red
    tuning(vectorSet, fixedNeigh, mapConfig.tuneIter)

    // Agrupa las entradas en el mapa ya organizado
    vectorSet.vectors.foreach(x => clusterInput(x))

    // Actualiza las metricas de la red
    somReady()
  }


  /**
   * Entrenamiento global de la red por un numero de iteraciones
   *
   * @param vectorSet Vectores de entrada usados para entrenamiento
   * @param trainIters Numero de iteraciones de entrenamiento
   */
  def roughTraining (
                      vectorSet: VectorSet,
                      trainIters: Int
                    ): Unit = {
    // Valores de radio y factor iniciales
    var decRadius = neighRadius
    var decFactor = learningFactor

    for (t <- 0 until trainIters) {
      // Obtiene el iterador sobre los vectores de entrada
      val setIt = vectorSet.iterator

      // Presenta todas las entradas al mapa
      while (setIt.hasNext) {
        // Obtiene el proximo vector a presentar
        val currentVector = setIt.next
        // Obtiene la BMU del vector actual
        val bmu = findBmu(currentVector.vector)

        // Aplica ciclo de entrenamiento centrado en la BMU
        lattice.neurons.flatten.foreach(x => {
          applySingleTraining(bmu._1.xPos, bmu._1.yPos, x, currentVector.vector, decFactor, decRadius)
        })
      }
      // Actualiza los factores al final de la iteracion
      decFactor = updateFactor(t, trainIters)
      decRadius = updateRadius(t, trainIters)
    }
  }


  /**
   * Fase de refinamiento de la red despues de que el orden global
   * se ha alcanzado
   *
   * @param vectorSet Vectores de entrada usados en el entrenamiento
   * @param fixedNeigh Valor de vecindad fijo
   * @param tuningIters Numero de iteraciones de refinamiento
   */
  def tuning (
               vectorSet: VectorSet,
               fixedNeigh: Double,
               tuningIters: Int
             ): Unit = {
    for (_ <- 0 until tuningIters) {
      // Obtiene el iterador sobre los vectores de entrada
      val setIt = vectorSet.iterator

      // Presenta todas las entradas al mapa
      while (setIt.hasNext) {
        // Obtain next vector to analyze
        val currentVector = setIt.next
        // Obtiene la BMU del vector actual
        val bmu = findBmu(currentVector.vector)

        // Refina la BMU y sus vecinas inmediatas
        applySingleTuning(bmu._1, currentVector.vector, fixedNeigh)
      }
    }
  }


  /**
   * Aplica la funcion de actualizacion de pesos a todas las neuronas
   * del mapa, centrada en la BMU de la entrada actual
   *
   * Emplea la formula:
   * wi (t + 1) = wi (t) + a(t) * hci(t) * dist(wi, vi)
   *
   * Donde hci es la funcion de vecindad cuyo valor maximo se alcanza
   * en la BMU y se reduce gradualmente con las distancias
   *
   * @param bmuX Coordenada X de la BMU en la distribucion de la grilla
   * @param bmuY Coordenada Y de la BMU en la distribucion de la grilla
   * @param unit Neurona a actualizar
   * @param inputVector Vector de pesos que fue representado por la BMU
   */
  def applySingleTraining (
                            bmuX: Float,
                            bmuY: Float,
                            unit: Neuron,
                            inputVector: Array[Double],
                            decFactor: Double,
                            decRadius: Double
                          ): Unit = {
    // Obtiene el vector de pesos de la neurona
    val weights = unit.weightVector

    // Actualiza cada dimension del vector de pesos
    for (i <- weights.indices) {
      val currentDim = weights(i)

      // Aplica la funcion de actualizacion
      weights.update(i, currentDim + decFactor * neighborhoodFn(bmuX, bmuY, unit.xPos, unit.yPos, decRadius) *
                    (inputVector(i) - currentDim))
    }
  }


  /**
   * Refina los pesos de una BMU y sus vecinas inmediatas
   *
   * Usa la formula:
   * wi (t + 1) = wi (t) + a(t) * dist(wi, vi)
   * para actualizar la BMU
   *
   * Y la formula:
   * wi (t + 1) = wi (t) + a(t) * hci * dist(wi, vi)
   * para actualizar a sus vecinas, donde hci es un valor de vecindad fijo para
   * la vecindad inmediata
   *
   * @param bmu BMU a refinar
   * @param inputVector Vector de entrada que fue representado por la BMU
   */
  def applySingleTuning (
                          bmu: Neuron,
                          inputVector: Array[Double],
                          neighValue: Double
                        ): Unit = {
    // Obtiene el vector de pesos de la BMU
    val vector = bmu.weightVector

    // Actualiza cada dimension del vector de pesos
    for (i <- vector.indices) {
      val currentDim = vector(i)

      // Aplica la funcion de actualizacion
      vector.update(i, currentDim + bmu.tuningRate * (currentDim - inputVector(i)))
    }
    // Actualiza el ratio de aprendizaje de la BMU
    bmu.updateTuningRate()

    // Aplica el refinamiento a cada una de las vecinas inmediatas
    bmu.neighbors.foreach(x => {
      // Obtiene el vector de pesos de la vecina
      val weights = x.weightVector

      // Actualiza cada dimension del vector de pesos
      for (i <- weights.indices) {
        val currentDim = weights(i)

        // Aplica la funcion de actualizacion con vecindad
        weights.update(i, currentDim + x.tuningRate * neighValue * (inputVector(i) - currentDim))
      }
      // Actualiza el ratio de aprendizaje de la vecina
      x.updateTuningRate()
    })
  }


  /**
   * Reduce de forma lineal, inversa del tiempo, el factor de aprendizaje de la fase
   * de enternamiento
   *
   * @param epoch Iteracion actual
   * @param totIter Total de iteraciones a efectuar
   * @return Factor de aprendizaje para la iteracion actual
   */
  def updateFactor (
                     epoch: Int,
                     totIter: Float
                   ): Double = {
    learningFactor * (1 - epoch / totIter)
  }

  /*
   * Gets y sets
   */

  def learningFactor: Double = _learningFactor
  
  def tuningFactor: Double = _tuningFactor
}
