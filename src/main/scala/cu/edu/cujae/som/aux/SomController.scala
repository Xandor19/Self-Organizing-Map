package cu.edu.cujae.som.aux

import java.util.concurrent.TimeUnit

import cu.edu.cujae.som.data.{InputVector, RandomVectorSet, VectorSet}
import cu.edu.cujae.som.function.InitFactory
import cu.edu.cujae.som.io.{ClusteringData, DetectionData, ExperimentData, MapConfig, MapIo, ReaderWriter, Tasks}
import cu.edu.cujae.som.map.{Som, SomFactory}

import scala.util.Random

/**
 * Objeto para controlar el flujo de procesamiento de un Som
 */
object SomController {

  /**
   * Controla el flujo de creacion y prueba de un nuevo Som
   *
   * @param config Parametros de configuracion para crear y probar el Som
   */
  def newSomFlow(config: MapConfig): Unit = {
    // Carga el dataset a emplear
    val dataset = ReaderWriter.loadSet(config.dataset)

    // Obtiene el nombre del dataset
    config.dataset = config.dataset.slice(config.dataset.lastIndexOf("/") + 1, config.dataset.lastIndexOf("."))

    // Prueba de agrupamiento
    if (config.task == Tasks.clustering) newClusterFlow(dataset, config)
    // Prueba de deteccion de anomalias
    else if (config.task == Tasks.anomaly) newAnomalyFlow(dataset, config)
  }


  /**
   * Flujo de creacion de Som para prueba de agrupamiento
   *
   * @param dataset Dataset cargado a emplear
   * @param config Parametros de configuracion
   */
  private def newClusterFlow (dataset: (Array[String], List[InputVector]), config: MapConfig): Unit = {
    // Obtiene la cantidad de experimentos
    val runs = config.runs

    // Array para guardar los modelos creados
    val created = new Array[(Som, Double)](runs)

    // Contenedores para las metricas
    var trainingAvMqe = 0.0
    var trainingMqeDeviation = 0.0
    var avgCorrect = 0.0
    var avgIncorrect = 0.0
    var avgPrecision = 0.0

    val expInitTime = System.nanoTime()
    for (test <- 0 until runs) {
      // Divide el dataset en set de entrenamiento y prueba
      val (trainingSet, testSet) = prepareForClust(dataset._1, dataset._2, config)

      // Completa la configuracion automaticamente si se requiere
      config.completeConfig(trainingSet.sampleSize)

      // Normaliza los datasets si se requiere
      if (config.normalize) { trainingSet.normalize(); testSet.normalize() }

      // Crea y entrena un nuevo Som
      val som = createSom(config, trainingSet)

      // Acumula las metricas de error del Som actual tras el entrenamiento
      val runTrainAvMqe = som.avMqe
      val runTrainMqeDeviation = som.sdMqe

      trainingAvMqe += runTrainAvMqe
      trainingMqeDeviation += runTrainMqeDeviation

      // Realiza la prueba de agrupamiento
      val results = clusterTest(som, testSet)

      // Guarda el modelo actual
      created.update(test, (som, results._3))

      // Acumula los resultados de la prueba
      avgCorrect += results._1
      avgIncorrect += results._2
      avgPrecision += results._3
    }
    if (runs > 1) {
      // Calcula los promedios de las metricas
      trainingAvMqe /= runs
      trainingMqeDeviation /= runs
      avgCorrect /= runs.toFloat
      avgIncorrect /= runs.toFloat
      avgPrecision /= runs
    }
    val expEndTime = System.nanoTime()
    // Obtiene el tiempo de ejecucion del experimento
    val seconds = TimeUnit.SECONDS.convert(expEndTime - expInitTime, TimeUnit.NANOSECONDS)


    if (config.exportTraining) {
      // Exporta los modelos entrenados con mejores resultados
      val origPath = config.trainingExportPath

      val lessMqe = origPath + "clust_less_Mqe_som_for_" + config.dataset + ".json"
      // Modelo que obtuvo menor Mqe tras entrenamiento
      ReaderWriter.exportTraining(
        lessMqe,
        prepareExport(
          config,
          created.minBy(x => x._1.avMqe)._1)
      )
      val greatPrec = origPath + "clust_great_prec_som_for_" + config.dataset + ".json"
      // Modelo que tuvo mayor precision en la prueba de agrupamiento
      ReaderWriter.exportTraining(greatPrec, prepareExport(config, created.minBy(x => x._2)._1))
    }

    // Construye el nombre del archivo de exportacion de resultados
    val exportName = "clust_results_of_" + config.dataset + ".csv"

    // Exporta los resultados del experimento
    ReaderWriter.exportExperimentResult(config.resultsExportPath + exportName,
                                        List[ExperimentData](new ClusteringData(config, trainingAvMqe, trainingMqeDeviation,
                                                             avgCorrect, avgIncorrect, avgPrecision,
                                                             String.format(seconds/60 + ":" + seconds%60))))
  }


  /**
   * Flujo de creacion de Som para prueba de deteccion de anomalias
   *
   * @param dataset Dataset cargado a emplear
   * @param config Parametros de configuracion
   */
  private def newAnomalyFlow (dataset: (Array[String], List[InputVector]), config: MapConfig): Unit ={
    // Obtiene la cantidad de experimentos
    val runs = config.runs

    // Array para guardar los modelos creados
    val created = new Array[(Som, Double)](runs)

    // Contenedores para las metricas
    var trainingAvMqe = 0.0
    var trainingMqeDeviation = 0.0
    var avgTruePos = 0.0
    var avgFalseNeg = 0.0
    var avgTrueNeg = 0.0
    var avgFalsePos = 0.0
    var avgConf = 0.0
    var avgSens = 0.0
    var avgAcc = 0.0

    val expInitTime = System.nanoTime()
    for (test <- 0 until runs) {
      // Divide el dataset en set de entrenamiento y prueba
      val (trainingSet, testSet) = prepareForAnom(dataset._1, dataset._2, config)

      // Completa la configuracion automaticamente si se requiere
      config.completeConfig(trainingSet.sampleSize)

      // Normaliza los datasets si se requiere
      if (config.normalize) { trainingSet.normalize(); testSet.normalize() }

      // Crea y entrena un nuevo Som
      val som = createSom(config, trainingSet)

      // Acumula las metricas de error del Som actual tras el entrenamiento
      val runTrainAvMqe = som.avMqe
      val runTrainMqeDeviation = som.sdMqe

      trainingAvMqe += runTrainAvMqe
      trainingMqeDeviation += runTrainMqeDeviation

      // Realiza la prueba de deteccion de anomalias
      val results = anomalyTest(som, testSet)

      // Calcula las metricas de la prueba
      val currentConf = results._1 / (results._1 + results._4).toFloat
      val currentSens = results._1 / (results._1 + results._2).toFloat
      val currentAcc = (results._1 + results._3) / (results._1 + results._2 + results._3 + results._4).toFloat

      // Guarda el modelo actual
      created.update(test, (som, currentAcc))

      // Acumula los resultados de la prueba
      avgTruePos += results._1
      avgFalseNeg += results._2
      avgTrueNeg += results._3
      avgFalsePos += results._4
      avgConf += currentConf
      avgSens += currentSens
      avgAcc += currentAcc
    }
    if (runs > 1) {
      // Calcula los promedios de las metricas
      trainingAvMqe /= runs
      trainingMqeDeviation /= runs
      avgTruePos /= runs.toFloat
      avgFalseNeg /= runs.toFloat
      avgTrueNeg /= runs.toFloat
      avgFalsePos /= runs.toFloat
      avgConf /= runs
      avgSens /= runs
      avgAcc /= runs
    }
    val expEndTime = System.nanoTime()
    // Obtiene el tiempo de ejecucion del experimento
    val seconds = TimeUnit.SECONDS.convert(expEndTime - expInitTime, TimeUnit.NANOSECONDS)

    if (config.exportTraining) {
      // Exporta los modelos entrenados con mejores resultados
      val origPath = config.trainingExportPath

      val lessMqe = origPath + "anom_less_Mqe_som_for_" + config.dataset + ".json"
      // Modelo que obtuvo menor Mqe tras entrenamiento
      ReaderWriter.exportTraining(lessMqe, prepareExport(config, created.minBy(x => x._1.avMqe)._1))
      // Modelo que tuvo mayor precision en la prueba de deteccion de anomalias
      val greatAcc = origPath + "anom_great_acc_som_for_" + config.dataset + ".json"
      ReaderWriter.exportTraining(greatAcc, prepareExport(config, created.minBy(x => x._2)._1))
    }
    // Construye el nombre del archivo de exportacion de resultados
    val exportName = "anom_results_of_" + config.dataset + ".csv"

    // Exporta los resultados del experimento
    ReaderWriter.exportExperimentResult(config.resultsExportPath + exportName,
                                        List[ExperimentData](new DetectionData(config, trainingAvMqe, trainingMqeDeviation,
                                                             avgTruePos, avgFalseNeg, avgTrueNeg, avgFalsePos, avgConf,
                                                             avgSens, avgAcc, String.format(seconds/60 + ":" + seconds%60))))
  }


  /**
   * Prepara los conjuntos de entrenamiento y pruebas para una prueba de agrupamiento, del dataset
   * original o de una muestra de este si se especifica
   *
   * @param features Atributos del dataset
   * @param inputVectors Instancias del dataset
   * @param config Parametros de configuracion
   * @return Tupla de (set de entrenamiento, set de prueba) como instancias de VectorSet
   */
  def prepareForClust (features: Array[String], inputVectors: List[InputVector], config: MapConfig): (VectorSet, VectorSet) = {
    // Instancia de Random
    val rand = new Random()
    rand.setSeed(config.shuffleSeed)

    // Prepara el dataset, obteniendo una muestra si se especifica
    val subSet = if (config.setProp < 1) Utils.stratified(inputVectors, config.setProp, config.shuffleSeed)._1
                 else rand.shuffle(inputVectors)

    // Obtiene los sets de entenamiento y prueba mediante muestreo estratificado
    val split = Utils.stratified(subSet, config.trainingProp, config.shuffleSeed)

    // Crea el set de entrenamiento
    val trainingSet = new RandomVectorSet(features, split._1, config.shuffleSeed)
    // Crea el set de pruebas
    val testSet = new VectorSet(features, split._2)

    (trainingSet, testSet)
  }


  /**
   * Prepara los conjuntos de entrenamiento y pruebas para una prueba de deteccion de
   * anomalias, del dataset original o de una muestra de este si se especifica
   *
   * Para la preparacion se obtiene una proporcion de las instancias normales como
   * set de entrenamiento y el resto e igual proporcion de las anomalias se dejan
   * como set de prueba
   *
   * @param features Atributos del dataset
   * @param inputVectors Instancias del dataset
   * @param config Parametros de configuracion
   * @return Tupla de (set de entrenamiento, set de prueba) como instancias de VectorSet
   */
  def prepareForAnom (features: Array[String], inputVectors: List[InputVector], config: MapConfig): (VectorSet, VectorSet) = {
    // Instancia de Random
    val rand = new Random()
    rand.setSeed(config.shuffleSeed)

    // Divide el dataset en la clase normal y la anomala
    val classes = Utils.splitByClasses(inputVectors).sortWith((x, y) => x.head.classification < y.head.classification)

    // Obtiene el tamaño del set de entrenamiento
    val normalProp = (classes.head.size * config.trainingProp).toInt
    // Barajea las instancias normales y las divide segun la proporcion
    val normal = rand.shuffle(classes.head).splitAt(normalProp)

    // Crea el set de entrenamiento con la proporcion de entradas normales dada
    val trainingSet = new RandomVectorSet(features, normal._1)

    // Obtiene la cantidad de anomalias a incluir
    val abnormalProp = (classes.last.size * config.trainingProp).toInt
    // Barajea las instancias anomalas y obtiene el subconjunto a emplear
    val abnormal = rand.shuffle(classes.last).slice(0, abnormalProp)

    // Crea el set de pruebas con la cantidad de anomalias y el resto de normales
    val testSet = new VectorSet(features, rand.shuffle(abnormal.appendedAll(normal._2)))

    (trainingSet, testSet)
  }


  /**
   * Crea un nuevo Som con la configuracion dada
   *
   * @param config Parametros de configuracion
   * @param trainingSet Set de entrenamiento
   * @return El Som creado, entrenado
   */
  def createSom (config: MapConfig, trainingSet: VectorSet): Som = {
    // Crea el Som del tipo y distribucion de grilla dado
    val som = SomFactory.createSom(config)

    // Establece el estado inicial del Som
    som.initSom(trainingSet, InitFactory(config.initFn), config.initSeed)

    // Entrena el Som
    som.organizeMap(trainingSet, config)

    som
  }


  /**
   * Maneja el flujo de la importacion de un modelo pre-entrenado y su evaluacion con un dataset
   *
   * @param configPath Ruta hacia el fichero .json con los parametros del modelo
   * @param datasetPath Ruta hacia el fichero .csv que contiene al dataset
   */
  def importSomFlow (configPath: String, datasetPath: String, resultPath: String): Unit = {
    // Parametros de entrenamiento del modelo
    val parameters = ReaderWriter.loadTraining(configPath)
    // Dataset a emplear
    val dataset = ReaderWriter.loadSet(datasetPath)
    // Set de prueba creado a partir del dataset
    val testSet = new VectorSet(dataset._1, dataset._2)
    // Som creado a partir del modelo importado
    val som = SomFactory.importSom(parameters)

    // Normaliza el set si se requiere
    if (parameters.normalized) testSet.normalize()

    // Prueba de agrupamiento
    if (parameters.task == Tasks.clustering) importClustFlow(som, testSet, parameters.dataset, resultPath)
    // Prueba de deteccion de anomalias
    else if (parameters.task == Tasks.anomaly) importAnomFlow (som, testSet, parameters.dataset, resultPath)
  }


  /**
   * Flujo de prueba de agrupamiento para un modelo importado
   * @param som Modelo pre-entrenado a evaluar
   * @param testSet Set de pruebas a emplear
   * @param setName Nombre del dataset a emplear
   * @param resultPath Ruta del fichero de exportacion de los resultados
   */
  private def importClustFlow (som: Som, testSet: VectorSet, setName: String, resultPath: String): Unit = {
    val init = System.nanoTime()
    // Realiza la prueba
    val results = clusterTest(som, testSet)
    val end = System.nanoTime()

    // Mide el tiempo de ejecucion de la prueba
    val seconds = TimeUnit.SECONDS.convert(end - init, TimeUnit.NANOSECONDS)
    val elapsed = String.format("%s:%s", seconds / 60, seconds % 60)

    // Construye el nombre del archivo de exportacion de resultados
    val expName = resultPath + "import_clust_of_" + setName + ".csv"

    // Exporta los resultados del experimento
    ReaderWriter.exportExperimentResult(expName, List[ExperimentData]
      (new ClusteringData(null, som.avMqe, som.sdMqe, results._1, results._2, results._3, elapsed)))
  }


  /**
   * Flujo de prueba de agrupamiento para un modelo importado
   * @param som Modelo pre-entrenado a evaluar
   * @param testSet Set de pruebas a emplear
   * @param setName Nombre del dataset a emplear
   * @param resultPath Ruta del fichero de exportacion de los resultados
   */
  private def importAnomFlow (som: Som, testSet: VectorSet, setName: String, resultPath: String): Unit = {
    val init = System.nanoTime()
    // Realiza la prueba
    val results = anomalyTest(som, testSet)
    val end = System.nanoTime()

    // Obtiene las metricas de la prueba
    val conf = results._1 / (results._1 + results._4).toFloat
    val sens = results._1 / (results._1 + results._2).toFloat
    val acc = (results._1 + results._3) / (results._1 + results._2 + results._3 + results._4).toFloat

    // Mide el tiempo de ejecucion de la prueba
    val seconds = TimeUnit.SECONDS.convert(end - init, TimeUnit.NANOSECONDS)
    val elapsed = String.format("%s:%s", seconds / 60, seconds % 60)

    // Construye el nombre del archivo de exportacion de resultados
    val expName = resultPath + "import_anom_of_" + setName + ".csv"

    // Exporta los resultados del experimento
    ReaderWriter.exportExperimentResult(expName, List[ExperimentData]
                                        (new DetectionData(null, som.avMqe, som.sdMqe, results._1, results._2,
                                        results._3, results._4, conf, sens, acc, elapsed)))
  }


  /**
   * Prueba de agrupameinto sobre un Som creado o importado
   *
   * @param som Modelo a evaluar
   * @param testSet Set de pruebas a emplear
   * @return Tupla de (agrupados correctamente, agrupados incorrectamente, precision)
   */
  def clusterTest (som: Som, testSet: VectorSet): (Int, Int, Double) = {
    var right = 0
    var wrong = 0

    val testIt = testSet.iterator

    // Presenta las instancias de prueba al Som
    while (testIt.hasNext) {
      val vector = testIt.next
      // Encuentra la BMU del vector actual
      val bmu = som.findBmu(vector.vector)._1
      // Obtiene la clase representada por la BMU
      val neuronClass = bmu.mainClass

      // Agrupamiento correcto
      if (vector.classification == neuronClass) right += 1
      // Agrupamiento incorrecto
      else wrong += 1
    }
    // Calcula la precision en la prueba
    val precision = (right / testSet.sampleSize.toDouble) * 100

    (right, wrong, precision)
  }


  /**
   * Prueba de deteccion de anomalias sobre un Som creado o importado
   *
   * @param som Modelo a evaluar
   * @param testSet Set de pruebas a emplear
   * @return Tupla de (detectadas, fallidas, normales omitidas, normales detectadas)
   */
  def anomalyTest (som: Som, testSet: VectorSet): (Int, Int, Int, Int) = {
    // Obtiene la cantidad de anomalias del set
    val anomAm = testSet.vectors.count(x => x.classification == "1")
    // Obtiene la cantidad de instancias normales del set
    val normAm = testSet.sampleSize - anomAm
    // Obtiene el umbral de anomalia del mapa
    val threshold = som.normalityThreshold
    // Contadores para las anomalias
    var detected = 0
    var falsePos = 0
    // Obtiene el iterador sobre el set de prueba
    val it = testSet.iterator

    while (it.hasNext) {
      // Obtiene el siguiente vector
      val vector = it.next

      // La instancia supera el umbral de normalidad
      if (som.findBmu(vector.vector)._2 > threshold) {
        // Instancia normal incorrectamente detectada
        if (vector.classification == "0") falsePos += 1
        // Anomalia correctamente detectada
        else if (vector.classification == "1") detected += 1
      }
    }
    // Obtiene el resto de parametros
    val anomMiss = anomAm - detected
    val trueNeg = normAm - falsePos

    (detected, anomMiss, trueNeg, falsePos)
  }


  /**
   * Prepara un objeto MapIO con la configuracion de un Som entrenado para exportar
   *
   * @param config Parametros de configuracion inicial del Som
   * @param som Som entrenado
   * @return Objeto MapIO con los datos de exportacion
   */
  def prepareExport (config: MapConfig, som: Som): MapIo = {
    new MapIo(config.dataset, config.task, config.somType, config.latDistrib, som.lattice.width, som.lattice.height,
              config.normalize, config.distanceFn, som.avMqe, som.sdMqe, som.lattice.neurons.flatten.map(x =>
              (x.weightVector.mkString(","), x.hits, x.balance.toString(), x.mainClass)))
  }
}
