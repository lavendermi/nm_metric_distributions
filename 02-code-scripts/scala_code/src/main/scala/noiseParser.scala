package ca.mikelavender.nm_metric_evaluations

import org.apache.commons.compress.utils.IOUtils

import java.io.{BufferedWriter, File, FileInputStream, FileWriter}
import scala.collection.mutable
// todo: this needs to be updated still
object noiseParser extends App {

  val metricsList = List("beta_w", "beta_c", "beta_wb", "beta_r", "beta_I", "beta_e", "beta_t", "beta_m", "beta_co",
    "beta_cc", "beta_minus3", "williams", "beta_rlb", "beta_sim", "beta_gl", "beta_z",
    "A_1", "A_2", "A_3", "A_4", "A_5", "A_6", "A_7", "A_8", "A_9", "A_10", "A_11", "A_12", "A_13", "A_14", "A_15",
    "A_16", "A_17", "A_18", "A_19", "A_20", "A_21", "A_22", "A_23", "A_24", "A_25", "A_26", "A_27", "A_28", "A_29",
    "A_30", "A_31", "A_32", "A_33", "C_7", "C_8", "A_36", "A_37", "A_38", "A_39", "A_40", "A_41", "A_42", "A_43",
    "S_8", "S_26",
    "Index_2", "Index_3", "Index_4", "Index_5", "Index_6", "Index_7", "Index_8", "Index_9", "Index_12",
    "Index_16", "Index_18", "Index_41", "Index_46", "Index_45",
    "BR", "NODF", "Sharedness", "Togetherness", "checker", "clumping", "combo", "cscore", "netCovariance", "sCoE", "vRatio"
  )

  val neg_metrics = List("checker", "cscore", "beta_w", "beta_c", "beta_wb", "beta_r", "beta_I", "beta_e", "beta_t",
    "beta_m", "beta_co", "beta_cc", "beta_minus3", "beta_sim", "beta_gl", "beta_z", "S_8", "S_26", "Index_3", "Index_4",
    "Index_5", "Index_6", "Index_7", "Index_8", "Index_9", "Index_12", "NODF", "BR"
  )

  // This pathVar needs to be set to match the system it is running on! Unfortunately, the way I setup the project I
  // can't use relative paths. It should point to the folder that the "01-raw_data" folder is in.
//  val pathVar = "/Users/mikelavender/Documents/Source Code/RStudioProjects/inverted_nm_metric_evaluations/"
  val pathVar = "/Users/mikelavender/Documents/Source Code/GitHub/nm_metrics_distributions/"

  val file = new File(pathVar + "01-raw_data/noise_parsed/noise_parsed.tsv")
  val bw = new BufferedWriter(new FileWriter(file))

  writeToFile("mtxid\tstep\tmetric\tnm\tdist\tdir\tspecies\tplots\tobsMetric\tlt\tgt\tmean\tsd\tses\tne_NaN_count\ttimestamp")

  val in = pathVar + "01-raw_data/noise/noise.tar.gz"
  //  val fin = new TarArchiveInputStream(new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(in))))
  val fin = new FileInputStream(in)

  var mtxID_int = 1
  var mtxID_map = Map[String, Int]()

  val it = Unpacker.open(fin)
  it foreach { case (archiveEntry, is) =>
    val nsFlagMap = collection.mutable.Map(metricsList.map(id => (id, mtxID_int)): _*)

    val nInaRow = collection.mutable.Map(metricsList.map(id => (id, 0)): _*)

    val tarFile = scala.io.Source.fromBytes(IOUtils.toByteArray(is))
    val lines = tarFile.getLines().drop(1)
    for (line <- lines) {
      val splitLine = line.split("\t")
      val mtxID = splitLine(0)
      val stepID = splitLine(1).toInt
      val currentMetric = splitLine(2)
      val isSig = isSignificant(splitLine)

      if(!mtxID_map.contains(mtxID)) {
        mtxID_map += (mtxID -> mtxID_int)
        mtxID_int = mtxID_int + 1
      }

      if (!isSig & nsFlagMap(currentMetric) == mtxID_map(mtxID)) nInaRow(currentMetric) = nInaRow(currentMetric) + 1 else nInaRow(currentMetric) = 0

      if (!isSig & nsFlagMap(currentMetric) == mtxID_map(mtxID) & nInaRow(currentMetric) == 5) {
        //       if (!isSig & nsFlagMap(currentMetric) == mtxID) {
        nsFlagMap(currentMetric) += 1
//        println(line)
        writeToFile(line)
      }
      if (nsFlagMap(currentMetric) < mtxID_map(mtxID)) nsFlagMap(currentMetric) = mtxID_map(mtxID)
    }
  }

  def isSignificant(parts: Array[String]): Boolean = {
    if (List("�", "∞", "-∞", "?").contains(parts(9)) | List("�", "∞", "-∞", "?").contains(parts(10))) {
      false
    } else {
      parts(5) match {
        case "negative" => {
          if (neg_metrics.contains(parts(2))) parts(9).toDouble >= 0.95 else parts(10).toDouble >= 0.95
        }
        case "positive" => {
          if (neg_metrics.contains(parts(2))) parts(10).toDouble >= 0.95 else parts(9).toDouble >= 0.95
        }
      }
    }
  }

  private def writeToFile(data: String): Unit = {
    bw.write(data + "\n")
    bw.flush()
  }

}
