package cu.edu.cujae.som.io

import cu.edu.cujae.som.map.Neuron

/**
 * Clase para almacenar los parametros de un SOM importado
 *
 * @param dataset Dataset a emplear en el SOM
 * @param task Tipo de prueba que se llevara a cabo
 * @param somType Tipo de SOM por enfoque de entrenamiento
 * @param latDistrib Distribucion de la grilla
 * @param width Ancho de la grilla
 * @param height Altura de la grilla
 * @param normalized Indicador de si el dataset debe ser normalizado o no
 * @param distFn Funcion de distancia empleada en el SOM
 * @param avMqe Media del Error de Cuantificacion Minimo de la red tras el entrenamiento
 * @param sdMqe Desviacion estandar del Error de Cuantificacion Minimo
 * @param neurons Estado de las neuronas tras el entrenamiento, referido como el vector de pesos
 *                e indicadores acerca de la representacion de las entradas
 */
class MapIo(val dataset: String, val task: String, val somType: String, val latDistrib: String, val width: Int,
            val height: Int, val normalized: Boolean, val distFn: String, val avMqe: Double, val sdMqe: Double,
            val neurons: Array[(String, Int, String, String)]) {

  /**
   * Convierte la informacion contenida de las neuronas hacia un array de estas
   * @return Array de Neuron con los valores de las neuronas guardadas
   */
  def strToNeurons: Array[Neuron] = {
    neurons.map(current => {
      val vector = current._1.split(",").map(_.toDouble)
      val splitBal = current._3.substring(1, 4).split(",").map(_.toInt)
      val balance = (splitBal.head, splitBal.last)

      new Neuron(0, 0, vector, current._2, balance, current._3)
    })
  }
}