package cu.edu.cujae.som.map

/**
 * Clase para representar una grilla de distribucion hexagonal
 *
 * @param _width Ancho de la grilla
 * @param _height Altura de la grilla
 */
class HexLattice protected [map] (
                                   _width: Int,
                                   _height: Int
                                 )
  extends Lattice (_width, _height) {

  /**
   * Funcion abstracta para obtener las coordenadas de una neurona segun
   * la distribucion de la grilla
   *
   * @param i Indice de fila en la grilla
   * @param j Indice de columna en la grilla
   * @return Tupla (coordenada X, coordenada Y)
   */
  override def coordFromIndex (
                                i: Int,
                                j: Int
                              ): (Float, Float) = {
    // Seno y coseno de 60º, empleados para la conversion de coordenadas
    val c60 = math.cos(math.Pi / 3)
    val s60 = math.sin(math.Pi / 3)

    // Fila par
    if (i % 2 == 0) (j, i * s60.toFloat)
    // Fila impar
    else (j + c60.toFloat, i *s60.toFloat)
  }


  /**
   * Funcion abstracta para agregar vecinos a una neurona
   *
   * @param neuron Neurona a agregar sus vecinos
   * @param i Indice de fila en la grilla
   * @param j indice de columna en la grilla
   */
  override def addNeighbors (
                              neuron: Neuron,
                              i: Int,
                              j: Int
                            ): Unit = {
    if (j > 0) neurons(i)(j).addNeighbor(neurons(i)(j - 1))

    if (i % 2 == 0) {
      // Fila par
      if (i > 0 && j > 0) neurons(i)(j).addNeighbor(neurons(i - 1)(j - 1))
      if (i > 0) neurons(i)(j).addNeighbor(neurons(i - 1)(j))
    }
    else {
      // Fila impar
      if (i > 0) neurons(i)(j).addNeighbor(neurons(i-1)(j))
      if (i > 0 && j < height - 1) neurons(i)(j).addNeighbor(neurons(i-1)(j+1))
    }
  }


  /**
   * Proporciona el identificador del tipo de grilla actual
   *
   * @return Constante de LatticeDistribution que representa el tipo de distribucion
   */
  override def latticeType: String = LatticeDistribution.Hexagonal
}
