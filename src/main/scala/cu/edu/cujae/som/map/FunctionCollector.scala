package cu.edu.cujae.som.map

import cu.edu.cujae.som.data.VectorSet

import scala.util.Random

/**
 * Objeto que contiene varias funciones empleadas por el SOM
 *
 */
object FunctionCollector {

  /**
   * Proporciona una funcion de inicializacion especifica dado su identificador
   *
   * @param init Identificador de la funcion de inicializacion
   * @return Funcion deseada o null si no existe el identificador
   */
  def initFactory (init: String): (Iterable[Array[Double]], VectorSet, Long) => Unit = {
    if (init == InitFns.randomInit) randomInit
    else if (init == InitFns.normalizedRandomInit) normalizedRandomInit
    else null
  }


  /**
   * Proporciona una funcion de distancia especifica dado su identificador
   *
   * @param dist Identificador de la funcion de distancia
   * @return Funcion deseada o null si no existe el identificador
   */
  def distanceFactory (dist: String): (Array[Double], Array[Double]) => Double = {
    if (dist == DistanceFns.simpleEuclidean) euclideanDistance
    else if (dist == DistanceFns.squaredEuclidean) squaredEuclideanDistance
    else null
  }


  /**
   * Proporciona una funcion de vecindad especifica dado su identificador
   *
   * @param neigh Identificador de la funcion de vecindad
   * @return Funcion deseada o null si no existe el identificador
   */
  def neighboringFactory (neigh: String): (Float, Float, Float, Float, Double) => Double = {
    if (neigh == NeighboringFns.gaussian) gaussianNeighborhood
    else null
  }


  /**
   * Inicializa un set de vectores con valores aleatorios entre los limites
   * de las dimensiones del espacio de entrada
   *
   * @param vectors Vectores de peso a inicializar
   * @param vectorSet Espacio de entrada a utilizar en la inicializacion
   * @param seed Semilla para inicializacion aleatoria
   */
  def randomInit (vectors: Iterable[Array[Double]], vectorSet: VectorSet, seed: Long): Unit = {
    // Obtains bounds of the input's dimensions
    val bounds = vectorSet.dimBounds
    val rand = new Random()
    rand.setSeed(seed)

    vectors.foreach(x => {
      // Generates value for each dimension
      for (i <- bounds.indices) x.update(i, rand.between(bounds(i)._1, bounds(i)._2))
    })
  }


  /**
   * Inicializa un set de vectores con valores aleatorios normalizados
   * entre los limites de las dimensiones del espacio de entrada
   *
   * @param vectors Vectores de peso a inicializar
   * @param vectorSet Espacio de entrada a utilizar en la inicializacion
   * @param seed Semilla para inicializacion aleatoria
   */
  def normalizedRandomInit (vectors: Iterable[Array[Double]], vectorSet: VectorSet, seed: Long): Unit = {
    // Obtains bounds of the input's dimensions
    val bounds = vectorSet.dimBounds
    val rand = new Random()
    rand.setSeed(seed)

    vectors.foreach(x => {
      // Generates value for each dimension
      for (i <- bounds.indices) {
        val dimMin = bounds(i)._1
        val dimMax = bounds(i)._2

        x.update(i, (rand.between(dimMin, dimMax) - dimMin) / (dimMax - dimMin) )
      }
    })
  }


  /**
   * Calcula la distancia euclideana simple de dos vectores de pesos
   * @param arr1 1er vector
   * @param arr2 2do vector
   * @return Valor de distancia
   */
  def euclideanDistance (arr1: Array[Double], arr2: Array[Double]): Double = {
    math.sqrt(squaredEuclideanDistance(arr1, arr2))
  }


  /**
   * Calcula la distancia euclideana cuadrada de dos vectores de pesos
   * @param arr1 1er vector
   * @param arr2 2do vector
   * @return Valor de distancia
   */
  def squaredEuclideanDistance (arr1: Array[Double], arr2: Array[Double]): Double = {
    (arr1 zip arr2).map(x => math.pow(x._1 - x._2, 2)).sum
  }


  /**
   * Funcion de vecindad para reducir el impacto de un vector de entrada
   * conforme las neuronas se alejan de su BMU
   *
   * Emplea la funcion gaussian:
   * exp(squaredDistance(BMU, neighbor) / 2 * currentRadius**2
   *
   * @param bmuX Coordenada X de la BMU
   * @param bmuY Coordenada Y de la BMU
   * @param neighX Coordenada X de la neurona vecina
   * @param neighY Coordenada Y de la neurona vecina
   * @param neighRadius Radio de vecindad actual
   * @return Valor de vecindad para la neurona con respecto a la BMU
   */
  def gaussianNeighborhood (bmuX: Float, bmuY: Float, neighX: Float, neighY: Float, neighRadius: Double): Double = {
    val distance = squaredEuclideanDistance(Array(bmuX, bmuY), Array(neighX, neighY))

    if (distance == 0) 1
    else {
      val value = math.exp( (-distance) / (2 * math.pow(neighRadius, 2)))

      if (value.isNaN) 0 else value
    }
  }
}


/**
 * Identificadores para las funciones de inicializacion
 */
object InitFns {
  val randomInit = "Random"
  val normalizedRandomInit = "Normalized Random"
}


/**
 * Identificadores para las funciones de distancia
 */
object DistanceFns {
  val simpleEuclidean = "Euclidean"
  val squaredEuclidean = "Squared Euclidean"
}


/**
* Identificadores para las funciones de vecindad
*/
object NeighboringFns {
  val gaussian = "Gaussian"
}