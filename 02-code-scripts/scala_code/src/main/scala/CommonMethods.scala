package ca.mikelavender.nm_metric_evaluations

import ca.mikelavender.associations.algorithms.NullModels
import ca.mikelavender.associations.metrics.cooccurrence.{abcd_metrics, matrix_metrics}

import java.io.BufferedWriter
import scala.collection.mutable
import scala.math.sqrt

class CommonMethods {

  //  val metrics = new CoOccurrenceIndices
  val abcd_metrics = new abcd_metrics
  val matrix_metrics = new matrix_metrics

  /**
   * inverts binary presence-absence matrix so that 1's are abscences and 0's are presences
   *
   * @param mtxOrig
   * @return Array[Array[Int]]
   */
  def invertMatrix(mtxOrig: Array[Array[Int]]): Array[Array[Int]] = {
    mtxOrig.map(_.clone()).map(_.map {
      case 1 => 0
      case 0 => 1
    }
    )
  }

  def runNullModels(mtxOrig: Array[Array[Int]], sim: String, sim9Iter: Int, metricsList: List[String]): mutable.Map[String, List[Double]] = {
    val nm = new NullModels

    var nullMap: mutable.Map[String, List[Double]] = mutable.Map[String, List[Double]]().withDefaultValue(List[Double]())
    for (i <- 1 to 1000) {
      //      println(i)
      val shuffled = sim match {
        case "sim1" => nm.sim1(mtxOrig.map(_.clone()))
        case "sim2" => nm.sim2(mtxOrig.map(_.clone()))
        case "sim3" => nm.sim3(mtxOrig.map(_.clone()))
        case "sim4" => nm.sim4(mtxOrig.map(_.clone()))
        case "sim5" => nm.sim5(mtxOrig.map(_.clone()))
        case "sim6_eq" => nm.sim6_equiprob(mtxOrig.map(_.clone()))
        case "sim6" => nm.sim6(mtxOrig.map(_.clone()))
        case "sim7_eq" => nm.sim7_equiprob(mtxOrig.map(_.clone()))
        case "sim7" => nm.sim7(mtxOrig.map(_.clone()))
        case "sim8" => nm.sim8(mtxOrig.map(_.clone()))
        case "sim9" => nm.sim9(mtxOrig.map(_.clone()), iterations = 5 * List(mtxOrig.length, mtxOrig(0).length).min)
        case "pp" => nm.pp(mtxOrig.map(_.clone()))
        case _ => sys.exit(1)
      }

      for (metric <- metricsList) {
        nullMap += (metric -> (getMetricValue(shuffled, metric).get +: nullMap(metric)))
      }
    }
    nullMap
  }

  def getMetricValue(testMatrix: Array[Array[Int]], metric: String): Option[Double] = {
    if (List("sCoE", "netCovariance", "NODF", "BR", "clumping", "combo", "vRatio", "checker").contains(metric)) {
      metric match {
        case "sCoE" => matrix_metrics.scaledCoEvenness(testMatrix)
        case "netCovariance" => matrix_metrics.netCovariance(testMatrix)
        case "NODF" => matrix_metrics.NODF(testMatrix)
        case "BR" => matrix_metrics.BR(testMatrix)
        case "clumping" => matrix_metrics.Clumping(testMatrix)
        case "combo" => matrix_metrics.combo(testMatrix)
        case "checker" => matrix_metrics.checker(testMatrix)
        case "vRatio" => matrix_metrics.vRatio(testMatrix)
      }
    } else {
      //loop through all pairs and get average
      val result: Seq[Double] = for (i <- testMatrix.indices; j <- i + 1 until testMatrix.length) yield {
        val contingency_table = abcd_metrics.get_cont_table(Array(testMatrix(i), testMatrix(j)))

        val a = contingency_table._1
        val b = contingency_table._2
        val c = contingency_table._3
        val d = contingency_table._4

        val pair_score = metric match {
          case "a" => Option(a) // this is the same as Sharedness
          case "b" => Option(b)
          case "c" => Option(c)
          case "d" => Option(d)
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
          case "Sharedness" => abcd_metrics.Sharedness(a, b, c, d)
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
          case "Index_12_b" => abcd_metrics.Index_12_b(a, b, c, d)
          case "Index_16" => abcd_metrics.Index_16(a, b, c, d)
          case "Index_18" => abcd_metrics.Index_18(a, b, c, d)
          case "Index_41" => abcd_metrics.Index_41(a, b, c, d)
          case "Index_46" => abcd_metrics.Index_46(a, b, c, d)
          case "Index_45" => abcd_metrics.Index_45(a, b, c, d)
        }
        pair_score.get
      }

      Option(mean(result.toList))
    }
  }


  def writeToFile(bw: BufferedWriter, data: String): Unit = {
    bw.write(data)
    bw.flush()
  }

  def stdDev(numbers: List[Double]): Double = {

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

  def mean(numbers: List[Double]): Double = numbers.sum / numbers.length
}
