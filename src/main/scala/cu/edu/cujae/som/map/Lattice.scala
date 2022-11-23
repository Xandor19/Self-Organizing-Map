package cu.edu.cujae.som.map

import cu.edu.cujae.som.io.MapIo

/**
 * Clase para representar una grilla bi-dimensional
 *
 * @param _width Ancho de la grilla
 * @param _height Altura de la grilla
 */
abstract class Lattice (private val _width: Int, private val _height: Int) {

  /*
   * Atributos de la clase
   */
  private val _neurons: Array[Array[Neuron]] = Array.ofDim[Neuron](width, height)


  /**
   * Funcion para la construccion de una grilla
   * La distribucion de las coordenadas y vecindad se modifica de acuerdo
   * al tipo de grilla
   *
   * @param vectors Vectores a colocar como vectores de peso de las neuronas
   */

  def constructLattice (vectors: Iterable[Array[Double]]): Unit = {
    // Obtiene el iterador sobre los vectores
    val it = vectors.iterator

    for (i <- 0 until width; j <- 0 until height) {
      // Obtiene los indices para la neurona actual
      val indexes = coordFromIndex(i, j)

      // Crea la neurona
      neurons(i)(j) = new Neuron(indexes._1, indexes._2, it.next())

      // Asigna los vecinos a la neurona
      addNeighbors(neurons(i)(j), i, j)
    }
  }


  /**
   * Crea una grilla a partir de la configuracion de un modelo previamente entrenado
   * La distribucion de las coordenadas y vecindad se modifica de acuerdo
   * al tipo de grilla
   *
   * @param neuronData Parametros del modelo importado
   */
  def loadLattice (neuronData: MapIo): Unit = {
    // Obtiene el iterador sobre los datos de las neuronas
    val neuIt = neuronData.strToNeurons.iterator

    // Incializa cada neurona
    for (i <- 0 until width; j <- 0 until height) {
      // Neurona actual
      val current = neuIt.next()
      // Obtiene los indices para la neurona actual
      val indexes = coordFromIndex(i, j)

      // Crea la neurona a partir de los datos recibidos
      neurons(i)(j) = new Neuron(indexes._1, indexes._2, current.weightVector, current.hits,
                                 current.balance, current.mainClass)

      // Asigna los vecinos a la neurona
      addNeighbors(neurons(i)(j), i, j)
    }
  }


  /**
   * Funcion abstracta para obtener las coordenadas de una neurona segun
   * la distribucion de la grilla
   *
   * @param i Indice de fila en la grilla
   * @param j Indice de columna en la grilla
   * @return Tupla (coordenada X, coordenada Y)
   */
  def coordFromIndex (i: Int, j: Int): (Float, Float)


  /**
   * Funcion abstracta para agregar vecinos a una neurona segun la distribucion
   * de la grilla
   *
   * @param neuron Neurona a agregar sus vecinos
   * @param i Indice de fila en la grilla
   * @param j indice de columna en la grilla
   */
  def addNeighbors (neuron: Neuron, i: Int, j: Int): Unit


  /**
   * Proporciona una representacion de la grilla con la cantidad de vectores de
   * entrada que representa cada neurona
   *
   * @return Array bi-dimensional de enteros en que cada elemento representa la
   *         cantidad de representados por la neurona en la posicion
   */
  def neuronHits: Array[Array[Int]] = neurons.map(x => x.map(y => y.hits))


  /**
   * Proporciona una representacion de la grilla con el balance de clases de cada neurona
   *
   * @return Array bi-dimensional de tuplas (Cantidad de entradas, cantidad de clases) en que
   *         cada elemento representa la neurona en la posicion
   */
  def classesBalance: Array[Array[(Int, Int)]] = neurons.map(x => x.map(y => y.balance))


  /**
   * Proporciona una representacion de la grilla con la clase principal de cada neurona
   *
   * @return Array bi-dimensional de String en que cada elemento representa la clase
   *         principal de la neurona en la posicion
   */
  def mainClasses: Array[Array[String]] = neurons.map(x => x.map(y => y.mainClass))


  /**
   * Proporciona los vectores de pesos de las neuronas de esta grilla con la posicion de
   * la neurona que los contiene
   *
   * @return Array de tuplas (coordenada x, coordenada y, vector de pesos) para cada neurona
   */
  def indexedVectors: Array[(Float, Float, Array[Double])] = neurons.flatten.map(x => (x.xPos, x.yPos, x.weightVector))


  /**
   * Proporciona el identificador del tipo de grilla actual
   *
   * @return Constante de LatticeDistribution que representa el tipo de distribucion
   */
  def latticeType: String


  /*
   * Gets y Sets
   */

  def width: Int = _width


  def height: Int = _height


  def neurons: Array[Array[Neuron]] = _neurons
}


/**
 * Objeto para la creacion de una grilla segun la distribucion especificada
 */
object LatticeFactory {
  /**
   * Crea una grilla de la distribucion recibida con los parametros especificados
   *
   * @param latDistrib Id representing the desired distribution
   * @param width Ancho de la grilla
   * @param height Altura de la grilla
   * @return Grilla creada en estado inicial
   */
  def createLattice (latDistrib: String, width: Int, height: Int): Lattice = {
    latDistrib match  {
      case LatticeDistribution.rectangular => new RectLattice(width, height)
      case LatticeDistribution.hexagonal => new HexLattice(width, height)
      case _ => null
    }
  }
}


/**
 * Identificadores para las distribuciones de grillas
 */
object LatticeDistribution {
  val rectangular = "Rectangular"
  val hexagonal = "Hexagonal"
}
