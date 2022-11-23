package cu.edu.cujae.som.map

import cu.edu.cujae.som.aux.Utils
import cu.edu.cujae.som.data.InputVector

/**
 * Clase para representar una neurona de un SOM
 *
 * @param _xPos Coordenada X en la distribucion de la grilla
 * @param _yPos Coordenada Y en la distribucion de la grilla
 * @param _weightVector Vector de pesos de la neurona
 * @param _hits Cantidad de entradas que fueron representadas por la neurona
 * @param _balance Cantidad de entradas representadas por la neurona y cantidad de clases de estas
 * @param _mainClass Clase principal (con mas ocurrencias) en la neurona
 */
class Neuron (private val _xPos: Float, private val _yPos: Float, private var _weightVector: Array[Double],
              private var _hits: Int = 0, private var _balance: (Int, Int) = (0, 0),
              private var _mainClass: String = "None") {
  /*
   * Atributos de la clase
   */
  private var _tuningRate = 0.0
  private var _representedInputs = Map.empty[InputVector, Double]
  private var _neighbors = List.empty[Neuron]


  /**
   * Agrega el vector de entrada recibido entre los representados por esta neurona
   *
   * @param inputVector Vector a ser representado por la neurona
   */
  def representInput (inputVector: InputVector, qe: Double): Unit = {
    _representedInputs = _representedInputs.updated(inputVector, qe)
  }


  /**
   * Obtiene la media de los vectores representados por esta neurona
   *
   * @return Vector medio de las entradas representadas
   */
  def meanVector: Array[Double] = {
    var mean = new Array[Double](weightVector.length)

    if (_representedInputs.nonEmpty) {
      _representedInputs.keys.map(x => x.vector).foreach(vector => {
        mean = mean.zip(vector).map(x => x._1 + x._2)
      })
      mean = mean.map(x => x / _representedInputs.size)
    }
    mean
  }


  /**
   * Actualiza la cantidad de entradas representadas por esta neurona
   */
  def updateHits (): Unit = _hits = _representedInputs.size


  /**
   * Cuenta cuantas clases diferentes son representadas por esta neurona y la cantidad de
   * vectores de estas
   *
   * @return Map cuyo par llave/valor es [Clase, cantidad de vectores]
   */
  def representedClasses: Map[String, Int] = Utils.classCount(_representedInputs.keys)


  /**
   * Actualiza el balance de clases de esta neurona
   */
  def updateBalance (): Unit = _balance = (_representedInputs.size, representedClasses.size)


  /**
   * Actualiza la clase principal de esta neurona, aquella con mayor cantidad de
   * vectores de entrada
   */
  def updateMainClass (): Unit = {
    // La neurona representa al menos un vector, se busca la clase principal
    if (_representedInputs.nonEmpty) _mainClass = representedClasses.toList.maxBy(x => x._2)._1
    // La neurona no representa a ningun vector
    else _mainClass = "None"
  }


  /**
   * Restablece las entradas representadas por la neurona
   */
  def clearRepresented(): Unit = _representedInputs = _representedInputs.empty


  /**
   * Agrega la neurona recibida como vecina inmediate de esta neurona
   * La vecindad es simetrica, esta neurona sera agregada como vecina de la recibida
   *
   * @param neigh Neurona a ser agregada como vecina
   * @return True si se completo la operacion, False en caso contraio
   */
  def addNeighbor (neigh: Neuron): Boolean = {
    // Comprueba que la neurona no tenga agregada a la recibida como vecina
    if (neigh != null && !_neighbors.contains(neigh)) {
      // Agrega la neurona como vecina inmediata y viceversa
      _neighbors = _neighbors.appended(neigh)
      neigh.addNeighbor(this)
      true
    }
    // No se completo la operacion
    else false
  }


  /**
   * Actualiza el factor de refinamiento de la neurona cuando es seleccionada como BMU
   * en la fase de refinamiento del aprendizaje on-line
   */
  def updateTuningRate (): Unit = {
    _tuningRate = _tuningRate / (1 + _tuningRate)
  }


  /*
   * Gets y Sets
   */

  def xPos: Float = _xPos

  def yPos: Float = _yPos

  def weightVector: Array[Double] = _weightVector
  def weightVector_= (vector: Array[Double]): Unit = _weightVector = vector


  def representedInputs: Map[InputVector, Double] = _representedInputs


  def tuningRate: Double = _tuningRate
  def tuningRate_= (tune: Double): Unit = _tuningRate = tune


  def hits: Int = _hits
  def hits_= (act: Int): Unit = _hits = act


  def balance: (Int, Int) = _balance


  def mainClass: String = _mainClass


  def neighbors: List[Neuron] = _neighbors
}