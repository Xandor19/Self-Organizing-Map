package cu.edu.cujae.som.data

/**
 * Clase par representar un vector de entrada (un registro de un dataset)
 *
 * @param _datasetIndex Indice del vector en el dataset original
 * @param _vector Valores de las dimensiones de la instancia
 * @param _classification Clase del registro
 */
class InputVector (private val _datasetIndex: Int, private val _vector: Array[Double],
                   private val _classification: String) {

  /**
   * Provee la dimensionalidad (cantidad de atributos del vector
   *
   * @return Entero representando la dimensionalidad
   */
  def dim: Int = _vector.length


  /*
   * Gets y Sets
   */

  def datasetIndex: Int = _datasetIndex


  def vector: Array[Double] = _vector


  def classification: String = _classification
}
