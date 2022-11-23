package cu.edu.cujae.som.io

import cu.edu.cujae.som.map.SomType

import scala.util.Random

/**
 * Clase para manipular los parametros de configuracion para los flujos de creacion
 * de nuevos SOM
 *
 * @param dataset Ruta del dataset a emplear en el entrenamiento/prueba
 * @param setProp Proporcion del dataset a emplear, si se desea solo una muestra de este
 * @param trainingProp Proporcion del conjunto final que se utilizara para entrenamiento
 * @param normalize Indicador de si se debe aplicar normalizacion o no
 * @param somType Tipo de enfoque de entrenamiento a usar en el SOM
 * @param latDistrib Distribucion de la grilla del SOM
 * @param width Ancho de la grilla
 * @param height Altura de la grilla
 * @param neighRadius Radio de vecindad inicial
 * @param learnFactor Factor de aprendizaje para entrenamiento on-line
 * @param tuneFactor Factor de refinamiento para entrenamiento on-line
 * @param initFn Funcion de inicializacion a emplear
 * @param distanceFn Funcion de distancia a emplear
 * @param neighFn Funcion de vecindad a emplear
 * @param trainIter Numero de iteraciones para entrenamiento estandar (ambos enfoques)
 * @param tuneIter Numero de iteraciones para refinamiento en el enfoque on-line
 * @param runs Cantidad de modelos a crear en la evaluacion
 * @param randInitSeed Semilla para llevar la traza de las inicializaciones aleatorias
 * @param randShuffleSeed Semilla para llevar la traza de los "barajeos" en estructuras secuenciales
 * @param task Tarea (tipo de prueba) a realizar por el SOM
 * @param resultsExportPath Ruta hacia el directorio de exportacion de los resultados de la prueba
 * @param trainingExportPath Ruta hacia el directorio de exportacion del modelo entrenado
 */
class MapConfig (var dataset: String, val setProp: Double, val trainingProp: Double,
                 val normalize: Boolean = true, val somType: String, val latDistrib: String, var width: Int = 0,
                 var height: Int = 0, var neighRadius: Int = 0, val learnFactor: Double = 0,
                 val tuneFactor: Double = 0, val initFn: String, val distanceFn: String, val neighFn: String,
                 var trainIter: Int = 0, var tuneIter: Int = 0, val runs: Int = 1,
                 val randInitSeed: Long = Random.nextInt(), val randShuffleSeed: Long = Random.nextInt(), val task: String,
                 var resultsExportPath: String = "", var trainingExportPath: String) {

  /*
   * Atributos de la clase
   */
  private val randomizedInitSeed = new Random()
  randomizedInitSeed.setSeed(randInitSeed)

  private val randomizedShuffledSeed = new Random()
  randomizedShuffledSeed.setSeed(randShuffleSeed)


  /**
   * Proporciona una string con los nombres de los parametros contenidos en la clase
   */
  val attributes: String = "Dataset,Dataset prop,Training Prop,Data normalized,SOM type,Lattice " +
    "distribution,Lattice width,Lattice height,Neighborhood radius,Learning factor,Tuning " +
    "factor,Initialization,Distance,Neighborhood function," +
    "Training iters,Tuning iters,Models created,Init seed,Set shuffling seed"


  /**
   * Proporciona una string con los valores de cada uno de los parametros especificados
   */
  def parameters: String = List(dataset, setProp, trainingProp, normalize, somType, latDistrib, width, height,
                                neighRadius, learnFactor, tuneFactor, initFn, distanceFn, neighFn,
                                trainIter, tuneIter, runs, randInitSeed, randShuffleSeed).mkString(",")


  /**
   * Proporciona una nueva semilla para inicializacion, a partir de la secuencia de valores de la semilla
   * de inicializacion original
   *
   * @return Entero aleatorio de la secuencia
   */
  def initSeed: Int = randomizedInitSeed.nextInt


  /**
   * Proporciona una nueva semilla para barajeo, a partir de la secuencia de valores de la semilla
   * de barajeo original
   *
   * @return Entero aleatorio de la secuencia
   */
  def shuffleSeed: Int = randomizedShuffledSeed.nextInt


  /**
   * Completa la configuracion del SOM de manera automatica mediante la cantidad de datos
   * de entrenamiento.
   * La distribucion de la grilla es generada si los valores especificados
   * para sus dimensiones son 0 (valor por omision al crear la clase)
   * La cantidad de iteraciones es generada en dependencia de los parametros del enfoque
   * y cuales son requeridos
   *
   * @param setSize Cantidad de instancias del set de entrenamiento
   */
  def completeConfig (setSize: Int): Unit = {
    var neurons: Double = width * height
    // Auto-configuracion del tamaño de la red si se requiere
    if (width == 0 || height == 0) {
      // Obtiene la cantidad de neuronas a partir de la cantidad de entradas
      neurons = math.sqrt(setSize) * 5
      // Adapta las proporciones de la grilla a la cantidad de neuronas
      height = math.sqrt(neurons).toInt

      width = if (neurons - math.pow(height, 2) > height / 2) height + 1
              else if (neurons - math.pow(height, 2) > height * 2) { height += 1; height }
              else height

      // Obtiene el radio de vecindad a partir de las proporciones
      neighRadius = width / 2 + 1
    }
    // Se requiere la cantidad de iteraciones
    if (trainIter == 0) {
      // Valor por omision para entrenamiento on-line
      if (somType == SomType.OnlineSom) trainIter = 1000
      // Valor por omision para entrenamiento en batch
      else trainIter = 200
    }
    if (tuneIter == 0 && tuneFactor > 0) {
      // Ajuste de la cantidad de iteraciones de refinamiento para el enfoque on-line
      tuneIter = (neurons * 500).toInt
    }
  }


  /**
   * Indica si se desea exportar o no el modelo entrenado
   *
   * @return Booleano para indicar la operacion
   */
  def exportTraining: Boolean = trainingExportPath != ""
}


/**
 * Objeto para indicar el tipo de tarea a realizar
 */
object Tasks {
  val clustering = "clustering"
  val anomaly = "anomaly"
}
