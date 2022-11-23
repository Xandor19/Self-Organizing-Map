package cu.edu.cujae.som.io

/**
 * Clase para almacenar y transferir los resultados de una prueba de agrupamiento
 *
 * @param config Parametros de configuracion empleados en el flujo
 * @param trainAvMqe Error de Cuantificacion Minimo medio obtenido tras el entrenamiento
 * @param trainAvSD Desviacion estandar del Error de Cuantificacion Minimo
 * @param avCorrect Promedio de instancias agrupadas correctamente en su clase
 * @param avIncorrect Promedio de instancias agrupadas incorrectamente en otras clases
 * @param avPrecision Promedio de precision al agrupar instancias
 * @param elapsedTime Tiempo de ejecucion de la prueba
 */
class ClusteringData (config: MapConfig, trainAvMqe: Double, trainAvSD: Double, avCorrect: Double, avIncorrect: Double,
                     avPrecision: Double, elapsedTime: String)
                    extends ExperimentData (config, trainAvMqe, trainAvSD, elapsedTime) {

  /**
   * Proporciona una string con los nombres de los datos contenidos en la clase
   */
  override def attributes: String = super.attributes + "," + "Av success,Av failure,Av Precision"


  /**
   * Proporciona una string con los valores de resultados almacenados actualmente
   */
  override def data: String = super.data + "," + List(avCorrect, avIncorrect, avPrecision).mkString(",")
}
