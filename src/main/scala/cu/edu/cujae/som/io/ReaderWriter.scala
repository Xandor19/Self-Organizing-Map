package cu.edu.cujae.som.io

import java.io.{File, FileInputStream, FileNotFoundException, FileOutputStream}
import java.util.Formatter

import cu.edu.cujae.som.data.InputVector
import net.maritimecloud.internal.core.javax.json.{Json, JsonObject}

/**
 * Objeto que proporciona funciones de carga y escritura a memoria externa
 */
object ReaderWriter {

  /**
   * Carga un archivo .csv generico hacia una lista de registros
   *
   * @param path Ruta hacia el fichero
   * @return Lista con las lineas individuales del fichero
   */
  def loadCSV (path: String): List[String] = {
    // Opens file
    val bufferedSource = io.Source.fromFile(path)
    // Read all lines from file
    val lines = bufferedSource.getLines().toList
    // Closes file
    bufferedSource.close()

    lines
  }


  /**
   * Carga un dataset de un fichero .csv
   *
   * El fichero debe comenzar por una fila con los nombres de los
   * atributos del dataset y las siguientes representan registros individuales.
   * La ultima columna debe contener las clases de las instancias y se separa del
   * resto de atributos al construir los vectores
   *
   * @param path Ruta hacia el fichero que contiene el dataset
   * @return Tuple of (inputs dimension, input features, inputs)
   */
  def loadSet (path: String): (Array[String], List[InputVector]) = {
    // Lee el csv
    val records = loadCSV (path)
    // Obtiene los atributos
    val features = records.head.split(",").map(_.trim)
    // Obtiene la dimensionalidad de los datos
    val dimensionality = features.length - 1
    // Lista de vectores vacia
    var vectors = List.empty[InputVector]
    //
    var rowIndex = 1

    // Recorre las instancias
    records.tail.foreach(i => {
      // Transforma el registro en una lista sin espacios de sus dimensiones
      val cleared = i.split(",").map(_.trim)

      // Crea el vector de entreada para el registro, especificando su indice original en el dataset y su clase
      vectors = vectors.appended(new InputVector(rowIndex, cleared.slice(0, dimensionality).map(_.toDouble), cleared.last))
      rowIndex += 1
    })
    (features, vectors)
  }


  /**
   * Carga la configuracion de un SOM previamente entrenado desde un fichero .json
   *
   * @param path Ruta al fichero con los datos del modelo
   * @return Instancia de MapIO con la configuracion del modelo
   */
  def loadTraining (path: String): MapIo = {
    // Lee el fichero
    val reader = Json.createReader(new FileInputStream(path)).readObject()

    // Extrae las propiedades individualmente
    val dataset = reader.getString("dataset")
    val task = reader.getString("task")
    val somType = reader.getString("type")
    val latDistrib = reader.getString("distrib")
    val width = reader.getInt("width")
    val height = reader.getInt("height")
    val normalized = reader.getBoolean("normalized")
    val distFn = reader.getString("dist_funct")
    val avMQE = reader.getJsonNumber("av_mqe").doubleValue()
    val sdMQE = reader.getJsonNumber("mqe_sd").doubleValue
    // Carga los datos de las neuronas en el formato de MapIO
    val neurons = reader.getJsonArray("neurons").toArray.
                                                 map(_.asInstanceOf[JsonObject]).
                                                 map(y => (y.getString("vector"), y.getInt("hits"),
                                                           y.getString("balance"), y.getString("class")))

    new MapIo(dataset, task, somType, latDistrib, width, height, normalized, distFn, avMQE, sdMQE, neurons)
  }


  /**
   * Exporta un modelo entrenado hacia un fichero .json
   * @param path Ruta al fichero de exportacion
   * @param data Parametros del modelo a exportar
   */
  def exportTraining (path: String, data: MapIo): Unit = {
    // Factory para crear los objetos de escritura
    val factory = Json.createBuilderFactory(null)
    // Array para escribir los datos de las neuronas
    val neurons = factory.createArrayBuilder()

    data.neurons.foreach(x => {
      // Da formato a los datos de una neurona
      neurons.add(factory.createObjectBuilder().
                  add("vector", x._1).
                  add("hits", x._2).
                  add("balance", x._3).
                  add("class", x._4).
                  build())
    })
    // Da formato a los parametros generales del modelo entrenado
    val export = factory.createObjectBuilder().
                 add("dataset", data.dataset).
                 add("task", data.task).
                 add("type", data.somType).
                 add("distrib", data.latDistrib).
                 add("width", data.width).
                 add("height", data.height).
                 add("normalized", data.normalized).
                 add("dist_funct", data.distFn).
                 add("av_mqe", data.avMqe).
                 add("mqe_sd", data.sdMqe).
                 add("neurons", neurons.build()).
                 build()

    // Escribe en el fichero
    val writer = Json.createWriter(new FileOutputStream(new File(path)))
    writer.write(`export`)
    // Cierra el fichero
    writer.close()
  }


  /**
   * Exporta el resultado de una o varias pruebas hacia un fichero .csv
   * @param path Ruta al fichero de registro
   * @param data Parametros del resultado a exportar
   */
  def exportExperimentResult (path: String, data: List[ExperimentData]): Unit = {
    // Lista para cargar el contenido anterior del fichero
    var existing = List.empty[String]
    var prev = true

    try {
      // Se intenta cargar el fichero
      existing = existing.appendedAll(loadCSV(path))
    }
    catch {
      // El fichero de destino no se ha creado
      case _: FileNotFoundException => prev = false
    }
    val writer = new Formatter(path)

    // Coloca el encabezado con los nombres de los parametros si el fichero es nuevo
    if (!prev) writer.format("%s\n", data.head.attributes)

    // Agrega los datos a exportar actualmente a los existentes
    existing = existing appendedAll data.map(x => x.data)

    // Escribe en el fichero
    existing.foreach(x => {
      writer.format("%s\n", x)
    })
    writer.flush()
    // Cierra el fichero
    writer.close()
  }
}
