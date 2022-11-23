package cu.edu.cujae.som.io

/**
 * Clase para almacenar y transferir los resultados de una prueba de deteccion de anomalias
 *
 * @param config Parametros de configuracion empleados en el flujo
 * @param trainAvMQE Error de Cuantificacion Minimo medio obtenido tras el entrenamiento
 * @param trainAvSD Desviacion estandar del Error de Cuantificacion Minimo
 * @param avTruePos Promedio de anomalias correctamente detectadas
 * @param avFalseNeg Promedio de anomalias no detectadas
 * @param avTrueNeg Promedio de instancias normales correctamente ignoradas
 * @param avFalsePos Promedio de instancias normales incorrectamente señaladas como anomalias
 * @param confidence Grado de confianza en la clasificacion otorgada por el SOM
 * @param sensibility Sensibilidad del SOM al detectar anomalias
 * @param accuracy Grado de precision general del SOM al clasificar una instancia
 * @param elapsedTime Tiempo de ejecucion de la prueba
 */
class DetectionData (config: MapConfig, trainAvMQE: Double, trainAvSD: Double, avTruePos: Double,
                     avFalseNeg: Double, avTrueNeg: Double, avFalsePos: Double, confidence: Double, sensibility: Double,
                     accuracy: Double, elapsedTime: String)
                    extends ExperimentData (config, trainAvMQE, trainAvSD, elapsedTime) {
  private val totAnom = avTruePos + avFalseNeg
  private val totNorm = avTrueNeg + avFalsePos

  /**
   * Proporciona una string con los nombres de los datos contenidos en la clase
   */
  override def attributes: String = super.attributes + "," + "Total anomalies,Total normal,Av true positives," +
                                                             "Av false negatives,Av true negatives," + "Av false " +
                                                             "positives,Av confidence,Av sensibility,Av accuracy"


  /**
   * Proporciona una string con los valores de resultados almacenados actualmente
   */
  override def data: String = super.data + "," + List(totAnom, totNorm, avTruePos, avFalseNeg, avTrueNeg, avFalsePos,
                                                      confidence, sensibility, accuracy).mkString(",")
}
