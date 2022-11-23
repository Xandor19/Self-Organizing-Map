package cu.edu.cujae.som.map

/**
 * Clase para representar una grilla de distribucion rectangular
 *
 * @param _width Ancho de la grilla
 * @param _height Altura de la grilla
 */
class RectLattice (_width: Int, _height: Int) extends Lattice (_width, _height) {

  /**
   * Funcion para obtener las coordenadas de una neurona segun
   * la distribucion de la grilla
   * En la distribucion rectangular los indices y coordenadas coinciden
   *
   * @param i Indice de fila en la grilla
   * @param j Indice de columna en la grilla
   * @return Tupla (coordenada X, coordenada Y)
   */
  override def coordFromIndex (i: Int, j: Int): (Float, Float) = (i, j)


  /**
   * Funcion para agregar vecinos a una neurona
   *
   * @param neuron Neurona a agregar sus vecinos
   * @param i Indice de fila en la grilla
   * @param j indice de columna en la grilla
   */
  override def addNeighbors (neuron: Neuron, i: Int, j: Int): Unit = {
    if (i > 0) neurons(i)(j).addNeighbor(neurons(i - 1)(j))
    if (j > 0) neurons(i)(j).addNeighbor(neurons(i)(j - 1))
  }


  /**
   * Proporciona el identificador del tipo de grilla actual
   *
   * @return Constante de LatticeDistribution que representa el tipo de distribucion
   */
  override def latticeType: String = LatticeDistribution.rectangular
}
