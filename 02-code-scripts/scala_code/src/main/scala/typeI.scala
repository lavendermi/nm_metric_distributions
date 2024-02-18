import java.io.{BufferedWriter, File, FileWriter}
import java.text.DecimalFormat
import ca.mikelavender.associations.algorithms.NullModels
import ca.mikelavender.associations.metrics.CoOccurrenceIndices
import ca.mikelavender.associations.metrics.cooccurrence.{abcd_metrics, matrix_metrics}
import ca.mikelavender.associations.utilities.MatrixFactory

import scala.collection.mutable
import scala.math.sqrt

class typeI(sim: String,
            sim9Iter: Int,
            species: Int,
            plots: Int,
            silent: Boolean,
            n: Int,
            distribution: String,
            pathVar: String) {

  // type I
  // create m x n matrix
  // run null models

  val formatter = new DecimalFormat("#0.00000")
  val sFormatter = new DecimalFormat("000000")

  val mFactory = new MatrixFactory
  val metrics = new CoOccurrenceIndices

  val abcd_metrics = new abcd_metrics
  val matrix_metrics = new matrix_metrics

  val file = new File(pathVar + "01-raw_data/typeI/typeI_" +
    sFormatter.format(new scala.util.Random().nextInt(100000)) + "_" +
    distribution + "_" + species + "x" + plots + "_" + sim + ".tsv")

  val bw = new BufferedWriter(new FileWriter(file))

  val metricsList = List("beta_w", "beta_c", "beta_wb", "beta_r", "beta_I", "beta_e", "beta_t", "beta_m", "beta_co",
    "beta_cc", "beta_minus3", "williams", "beta_rlb", "beta_sim", "beta_gl", "beta_z",
    "cscore", "Togetherness",
    "A_1", "A_2", "A_3", "A_4", "A_5", "A_6", "A_7", "A_8", "A_9", "A_10", "A_11", "A_12", "A_13", "A_14", "A_15",
    "A_16", "A_17", "A_18", "A_19", "A_20", "A_21", "A_22", "A_23", "A_24", "A_25", "A_26", "A_27", "A_28", "A_29",
    "A_30", "A_31", "A_32", "A_33", "C_7", "C_8", "A_36", "A_37", "A_38", "A_39", "A_40", "A_41", "A_42", "A_43",
    "S_8", "S_26", "Index_2", "Index_3", "Index_4", "Index_5", "Index_6", "Index_7", "Index_8", "Index_9", "Index_12",
    "Index_16", "Index_18", "Index_41", "Index_46", "Index_45",
    "sCoE", "netCovariance", "NODF", "BR", "clumping", "combo", "vRatio")

  val header = "id\tmetric\tnm\tdist\tspecies\tplots\tobsMetric\tlt\tgt\tmean\tsd\tses\n"

  if (!silent) print(header)
  writeToFile(header)
  var counter = 0

  for (loop <- 1 to n) {
    counter += 1
    val orgMtx = mFactory.buildMatrix(species, plots, speciesConstraint = distribution)

    val nullDist = runNullModels(orgMtx)

    for (metric <- metricsList) {
      val obsMetricValue = getMetricValue(orgMtx, metric)
      val brandMean = mean(nullDist(metric))
      val brandStDev = stdDev(nullDist(metric))
      val brandSES = (obsMetricValue.get - brandMean) / brandStDev
      val count = nullDist(metric).length.toDouble
      val lt = nullDist(metric).count(v => v < obsMetricValue).toDouble / count
      val gt = nullDist(metric).count(v => v > obsMetricValue).toDouble / count

      val outString = counter + "\t" + metric + "\t" + sim + "\t" + distribution + "\t" + species + "\t" + plots + "\t" +
        formatter.format(obsMetricValue) + "\t" +
        formatter.format(lt) + "\t" +
        formatter.format(gt) + "\t" +
        formatter.format(brandMean) + "\t" +
        formatter.format(brandStDev) + "\t" +
        formatter.format(brandSES) + "\n"

      if (!silent) print(outString)
      writeToFile(outString)

    }
  }

  private def getMetricValue(matrix: Array[Array[Int]], cont_table: (Double, Double, Double, Double), metric: String): Option[Double] = {
    val a = cont_table._1
    val b = cont_table._2
    val c = cont_table._3
    val d = cont_table._4

    metric match {
      case "beta_w" => abcd_metrics.beta_w(a, b, c, d)
      case "beta_c" => abcd_metrics.beta_c(a, b, c, d)
      case "beta_wb" => abcd_metrics.beta_wb(a, b, c, d)
      case "beta_r" => abcd_metrics.beta_r(a, b, c, d)
      case "beta_I" => abcd_metrics.beta_I(a, b, c, d)
      case "beta_e" => abcd_metrics.beta_e(a, b, c, d)
      case "beta_t" => abcd_metrics.beta_t(a, b, c, d)
      case "beta_m" => abcd_metrics.beta_m(a, b, c, d)
      case "beta_co" => abcd_metrics.beta_co(a, b, c, d)
      case "beta_cc" => abcd_metrics.beta_cc(a, b, c, d)
      case "beta_minus3" => abcd_metrics.beta_minus3(a, b, c, d)
      case "williams" => abcd_metrics.williams(a, b, c, d)
      case "beta_rlb" => abcd_metrics.beta_rlb(a, b, c, d)
      case "beta_sim" => abcd_metrics.beta_sim(a, b, c, d)
      case "beta_gl" => abcd_metrics.beta_gl(a, b, c, d)
      case "beta_z" => abcd_metrics.beta_z(a, b, c, d)
      case "cscore" => abcd_metrics.cscore(a, b, c, d)
      case "Togetherness" => abcd_metrics.Togetherness(a, b, c, d)
      case "A_1" => abcd_metrics.A_1(a, b, c, d)
      case "A_2" => abcd_metrics.A_2(a, b, c, d)
      case "A_3" => abcd_metrics.A_3(a, b, c, d)
      case "A_4" => abcd_metrics.A_4(a, b, c, d)
      case "A_5" => abcd_metrics.A_5(a, b, c, d)
      case "A_6" => abcd_metrics.A_6(a, b, c, d)
      case "A_7" => abcd_metrics.A_7(a, b, c, d)
      case "A_8" => abcd_metrics.A_8(a, b, c, d)
      case "A_9" => abcd_metrics.A_9(a, b, c, d)
      case "A_10" => abcd_metrics.A_10(a, b, c, d)
      case "A_11" => abcd_metrics.A_11(a, b, c, d)
      case "A_12" => abcd_metrics.A_12(a, b, c, d)
      case "A_13" => abcd_metrics.A_13(a, b, c, d)
      case "A_14" => abcd_metrics.A_14(a, b, c, d)
      case "A_15" => abcd_metrics.A_15(a, b, c, d)
      case "A_16" => abcd_metrics.A_16(a, b, c, d)
      case "A_17" => abcd_metrics.A_17(a, b, c, d)
      case "A_18" => abcd_metrics.A_18(a, b, c, d)
      case "A_19" => abcd_metrics.A_19(a, b, c, d)
      case "A_20" => abcd_metrics.A_20(a, b, c, d)
      case "A_21" => abcd_metrics.A_21(a, b, c, d)
      case "A_22" => abcd_metrics.A_22(a, b, c, d)
      case "A_23" => abcd_metrics.A_23(a, b, c, d)
      case "A_24" => abcd_metrics.A_24(a, b, c, d)
      case "A_25" => abcd_metrics.A_25(a, b, c, d)
      case "A_26" => abcd_metrics.A_26(a, b, c, d)
      case "A_27" => abcd_metrics.A_27(a, b, c, d)
      case "A_28" => abcd_metrics.A_28(a, b, c, d)
      case "A_29" => abcd_metrics.A_29(a, b, c, d)
      case "A_30" => abcd_metrics.A_30(a, b, c, d)
      case "A_31" => abcd_metrics.A_31(a, b, c, d)
      case "A_32" => abcd_metrics.A_32(a, b, c, d)
      case "A_33" => abcd_metrics.A_33(a, b, c, d)
      case "C_7" => abcd_metrics.C_7(a, b, c, d)
      case "C_8" => abcd_metrics.C_8(a, b, c, d)
      case "A_36" => abcd_metrics.A_36(a, b, c, d)
      case "A_37" => abcd_metrics.A_37(a, b, c, d)
      case "A_38" => abcd_metrics.A_38(a, b, c, d)
      case "A_39" => abcd_metrics.A_39(a, b, c, d)
      case "A_40" => abcd_metrics.A_40(a, b, c, d)
      case "A_41" => abcd_metrics.A_41(a, b, c, d)
      case "A_42" => abcd_metrics.A_42(a, b, c, d)
      case "A_43" => abcd_metrics.A_43(a, b, c, d)
      case "S_8" => abcd_metrics.S_8(a, b, c, d)
      case "S_26" => abcd_metrics.S_26(a, b, c, d)
      case "Index_2" => abcd_metrics.Index_2(a, b, c, d)
      case "Index_3" => abcd_metrics.Index_3(a, b, c, d)
      case "Index_4" => abcd_metrics.Index_4(a, b, c, d)
      case "Index_5" => abcd_metrics.Index_5(a, b, c, d)
      case "Index_6" => abcd_metrics.Index_6(a, b, c, d)
      case "Index_7" => abcd_metrics.Index_7(a, b, c, d)
      case "Index_8" => abcd_metrics.Index_8(a, b, c, d)
      case "Index_9" => abcd_metrics.Index_9(a, b, c, d)
      case "Index_12" => abcd_metrics.Index_12(a, b, c, d)
      case "Index_16" => abcd_metrics.Index_16(a, b, c, d)
      case "Index_18" => abcd_metrics.Index_18(a, b, c, d)
      case "Index_41" => abcd_metrics.Index_41(a, b, c, d)
      case "Index_46" => abcd_metrics.Index_46(a, b, c, d)
      case "Index_45" => abcd_metrics.Index_45(a, b, c, d)

      case "sCoE" => matrix_metrics.scaledCoEvenness(matrix)
      case "netCovariance" => matrix_metrics.netCovariance(matrix)
      case "NODF" => matrix_metrics.NODF(matrix)
      case "BR" => matrix_metrics.BR(matrix)
      case "clumping" => matrix_metrics.Clumping(matrix)
      case "combo" => matrix_metrics.combo(matrix)
      case "vRatio" => matrix_metrics.vRatio(matrix)
    }
  }

  private def runNullModels(mtxOrig: Array[Array[Int]]): mutable.Map[String, List[Double]] = {
    val nm = new NullModels

    var brandonsNullMap: mutable.Map[String, List[Double]] = mutable.Map[String, List[Double]]().withDefaultValue(List[Double]())
    for (i <- 0 to 1000) {
      val shuffled = sim match {
        case "sim1" => nm.sim1(mtxOrig.map(_.clone()))
        case "sim2" => nm.sim2(mtxOrig.map(_.clone()))
        case "sim3" => nm.sim3(mtxOrig.map(_.clone()))
        case "sim4" => nm.sim4(mtxOrig.map(_.clone()))
        case "sim5" => nm.sim5(mtxOrig.map(_.clone()))
        case "sim6" => nm.sim6(mtxOrig.map(_.clone()))
        case "sim7" => nm.sim7(mtxOrig.map(_.clone()))
        case "sim8" => nm.sim8(mtxOrig.map(_.clone()))
        case "sim9" => nm.sim9(mtxOrig.map(_.clone()), iterations = sim9Iter)
        case _ => sys.exit(1)
      }

      for (metric <- metricsList) {
        brandonsNullMap += (metric -> (getMetricValue(shuffled, metric) +: brandonsNullMap(metric)))
      }
    }
    brandonsNullMap
  }

  private def writeToFile(data: String): Unit = {
    bw.write(data)
    bw.flush()
  }

  private def stdDev(numbers: List[Double]): Double = {

    var sum: Double = 0.0
    if (numbers.length >= 2) {
      val avg = mean(numbers)
      val factor: Double = 1.0 / (numbers.length.toDouble - 1)
      for (x: Double <- numbers) {
        sum = sum + ((x - avg) * (x - avg))
      }
      sum = sum * factor
    }
    sqrt(sum)
  }

  private def mean(numbers: List[Double]): Double = numbers.sum / numbers.length
}
