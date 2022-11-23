package cu.edu.cujae.som.data

import scala.util.Random

/**
 * Clase para representar un conjunto de vectores de entrada que son
 * iterados en orden aleatorio cada vez
 *
 * @param features Nombres de los atributos de cada dimension de los vectores
 * @param vectors Conjunto de vectores
 */
class RandomVectorSet (features: Array[String], vectors: List[InputVector], seed: Long = Random.nextInt)
                      extends VectorSet (features, vectors) {

  /*
   * Atributos de la clase
   */
  private val rand = new Random()
  rand.setSeed(seed)


  /**
   * Proporciona un iterador sobre los vectores de entrada en orden aleatorio
   *
   * @return Instancia de SetIterator sobre los vectores "barajeados"
   */
  override def iterator: Iterator[InputVector] = new SetIterator(rand.shuffle(vectors))
}
