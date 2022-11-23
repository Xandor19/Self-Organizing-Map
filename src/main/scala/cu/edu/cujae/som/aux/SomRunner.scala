package cu.edu.cujae.som.aux

import cu.edu.cujae.som.function.{DistanceFns, InitFns, NeighboringFns}
import cu.edu.cujae.som.io.{MapConfig, Tasks}
import cu.edu.cujae.som.map._

import scala.util.Random

object SomRunner {

  def main(args: Array[String]): Unit = {
    val trainNew = true
    val datasetPath = "./Datasets/reduced_card_fraud_normalised_less_anomalies.csv"
    val modelImportPath = "./Pre-trained/anom_great_acc_som_for_reduced_card_fraud_normalised_less_anomalies.json"
    val modelExportPath = "./Pre-trained/"
    val resultsExportPath = "./Results/"
    val setProp = 1
    val trainingSetProp = 0.6
    var normalize = false
    val task = Tasks.anomaly
    var experiments = 30
    var somType = SomType.BatchSom
    var latDistrib = LatticeDistribution.Rectangular
    var latWidth = 0
    var latHeight = 0
    var latNeighRadius = 0
    var onlineLearningFactor = 0.8
    var onlineTuningFactor = 0.2
    var trainingIters = 500
    var onlineTuningIters = 27000
    var initFn = InitFns.RandomInit
    var distanceFn = DistanceFns.SquaredEuclidean
    var neighborhoodFn = NeighboringFns.Gaussian
    var initSeed = 152253427
    //Random.nextInt()
    var shuffleSeed = -1918061239
    //Random.nextInt()
    
    if (trainNew) {
      SomController.newSomFlow(new MapConfig(dataset = datasetPath, trainingExportPath = modelExportPath,
        resultsExportPath = resultsExportPath, setProp = setProp, trainingProp = trainingSetProp,
        normalize = normalize, task = task, runs = experiments, somType = somType,
        latDistrib = latDistrib, width = latWidth, height = latHeight, neighRadius = latNeighRadius,
        learnFactor = onlineLearningFactor, tuneFactor = onlineTuningFactor,
        trainIter = trainingIters, tuneIter = onlineTuningIters, initFn = initFn,
        distanceFn = distanceFn, neighFn = neighborhoodFn, randInitSeed = initSeed,
        randShuffleSeed = shuffleSeed))
    }
    else {
      SomController.importSomFlow(modelImportPath, datasetPath, resultsExportPath)
    }
  }
}
