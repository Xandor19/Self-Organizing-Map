package cu.edu.cujae.som.aux

import cu.edu.cujae.som.data.InputVector

import scala.util.Random

/**
 * Objeto para proporcionar diferentes funcionalidades de uso general
 */
object Utils {
  /**
   * Obtiene las diferentes clases de un conjunto de vectores con
   * el numero de ocurrencias de cada una
   *
   * @param inputs Vectores a analizar
   * @return Map de [Clase, cantidad de ocurrencias]
   */
  def classCount (inputs: Iterable[InputVector]): Map[String, Int] = {
    inputs.map(x => x.classification).foldLeft(Map.empty[String, Int]) {
      (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
    }
  }


  /**
   * Proporciona un muestreo estratificado para obtener una muestra de
   * un conjunto de vectores
   *
   * @param population Conjunto de vectores completo
   * @param prop Proporcion del conjunto a obtener como muestra
   * @param shuffleSeed Semilla para "barajeo" traceable
   * @return Tupla de (muestra deseada, resto de vectores)
   */
  def stratified (population: List[InputVector], prop: Double, shuffleSeed: Long = Random.nextInt):
                 (List[InputVector], List[InputVector]) = {
    // Instancia de random
    val rand = new Random()
    rand.setSeed(shuffleSeed)
    // Tamaño total del dataset y de la muestra deseada
    val popSize = population.size
    val sampleSize = (popSize * prop).toInt
    // Balance de clases
    val classes = classCount(population).map(x => (x._1, x._2.toDouble / popSize))
    // Listas para contener los resultados
    var sample = List.empty[InputVector]
    var rest = List.empty[InputVector]
    // Barajea la entrada
    val shuffled = rand.shuffle(population)

    // Agrega a la muestra la cantidad correspondiente de vectores de cada clase
    classes.foreach(x => {
      // Calculo de la cantidad de vectores
      val top = /*math.ceil*/(sampleSize * x._2).toInt
      // Instancias de la clase actual
      val ofClass = shuffled.filter(i => i.classification == x._1)

      // Se agrega a la muestra la cantidad correspondiente de vectores de la clase
      sample = sample.appendedAll(ofClass.slice(0, top))
      // Vectores restantes
      rest = rest.appendedAll(ofClass.slice(top, ofClass.size))
    })
    // Barajea el remanente
    rest = rand.shuffle(rest)

    (sample, rest)
  }


  /**
   * Divide un conjunto de vectores en las diferentes clases que lo conforman
   *
   * @param dataset Vectores a dividir
   * @return Lista de listas de vectores de cada clase
   */
  def splitByClasses (dataset: List[InputVector]): List[List[InputVector]] = {
    var distributed = List.empty[List[InputVector]]

    // Mapea los vectores en sus clases
    dataset.map(x => x.classification).distinct.foreach(x => {
      // Agreaga las clases de esta instancia a su lista
      distributed = distributed.appended(dataset.filter(v => v.classification == x))
    })
    distributed
  }
}
