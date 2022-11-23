package cu.edu.cujae.som.data

/**
 * Clase para representar un conjunto de vectores que define una serie
 * de operaciones sobre estos
 *
 * @param _features Nombres de los atributos de cada dimension de los vectores
 * @param _vectors Conjunto de vectores
 */
class VectorSet (private val _features: Array[String], private val _vectors: List[InputVector])
                extends Iterable[InputVector] {

  /*
   * Atributos de la clase
   */
  private val _dimensionality: Int = _features.length - 1
  private val _sampleSize: Int = _vectors.size
  private val _dimBounds: Array[(Double, Double)] = new Array[(Double, Double)](_dimensionality)
  private var _boundsFound = false


  /**
   * Obtiene los limites superior e inferior de cada dimension de los vectores
   */
  def findBounds (): Unit = {
    // Recorre los vectores por sus dimensiones
    for (i <- 0 until _dimensionality) {
      // Fija el minimo y maximo inicial como los valores del primer vector
      var minInDim = _vectors.head.vector(i)
      var maxInDim = minInDim

      // Recorre el resto de vectores
      _vectors.tail.foreach(x => {
        val current = x.vector(i)

        if (current < minInDim) {
          // Nuevo minimo encontrado, se actualiza
          minInDim = current
        }
        else if (current > maxInDim) {
          // Nuevo maximo encontrado, se actualiza
          maxInDim = current
        }
      })
      // Actualiza los limites de la dimension actual
      _dimBounds.update(i, (minInDim, maxInDim))
    }
    // Indica que se conocen los limites del conjunto de vectores
    _boundsFound = true
  }


  /**
   * Normaliza el conjunto de vectores actual
   */
  def normalize (): Unit = {
    // Halla los limites si no han sido encontrados
    if (!_boundsFound) findBounds()

    _vectors.foreach(x => {
      // Aplica la normalizacion en cada dimension
      for (i <- 0 until _dimensionality) {
        val currentLow = _dimBounds(i)._1
        val currentHigh = _dimBounds(i)._2

        // Formula para normalizacion
        x.vector.update(i, (x.vector(i) - currentLow) / (currentHigh - currentLow))
      }
    })
  }


  /**
   * Proporciona un iterador para las entradas en su orden original
   *
   * @return Instancia de SetIterator sobre el conjunto de vectores
   */
  override def iterator: Iterator[InputVector] = new SetIterator(_vectors)


  /*
   * Gets y Sets
   */

  def vectors: List[InputVector] = _vectors


  def dimensionality: Int = _dimensionality


  def sampleSize: Int = _sampleSize


  def dimBounds: Array[(Double, Double)] = {
    // Halla los limites si no han sido encontrados
    if (!_boundsFound) findBounds()
    _dimBounds
  }

  /**
   * Iterador sobre un conjunto de vectores de entrada
   *
   * @param vectors Vectores de entrada en el orden a iterar
   */
  protected class SetIterator (val vectors: List[InputVector]) extends Iterator[InputVector] {

    /*
     * Atributos de la clase
     */
    private var accessIndex = -1
    private val sampleSize = vectors.size


    /**
     * Comprueba si quedan vectores para iterar
     *
     * @return True si el indice de acceso no ha alcanzado el total de vectores,
     *         False en caso contrario
     */
    override def hasNext: Boolean = {
      accessIndex += 1

      if (accessIndex < sampleSize) true
      else false
    }


    /**
     * Proporciona el siguiente vector en el orden de iteracion
     *
     * @return InputVector correspondiente
     */
    override def next: InputVector = vectors(accessIndex)
  }
}
