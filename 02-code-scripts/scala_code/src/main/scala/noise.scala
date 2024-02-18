package ca.mikelavender.nm_metric_evaluations

import ca.mikelavender.associations.metrics.cooccurrence.{abcd_metrics, matrix_metrics}
import ca.mikelavender.associations.utilities.{MatrixFactory, MaximizeMetric}
import org.apache.commons.lang3.time.StopWatch

import java.io.{BufferedWriter, File, FileWriter}
import java.text.DecimalFormat
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class noise(sim: String,
            sim9Iter: Int,
            silent: Boolean,
            n: Int,
            distribution: String,
            direction: String,
            pathVar: String) {
  // noise
  // create m x n matrix
  // add maximal pattern to full matrix
  // run null models
  // add noise - repeat loop until random

  val formatter = new DecimalFormat("#0.00000")

  val mFactory = new MatrixFactory
  val commons = new CommonMethods

  val abcd_metrics = new abcd_metrics
  val matrix_metrics = new matrix_metrics

//  val file = new File(pathVar + "raw_data/noise/noise_" +
  val file = new File(pathVar + "raw_data/noise/inverted_noise_" +
    distribution + "_" + direction + "_" + sim + "_" + java.util.UUID.randomUUID.toString + ".tsv")

  val bw = new BufferedWriter(new FileWriter(file))

  val metricsList: List[String] = List("beta_w", "beta_c", "beta_wb", "beta_r", "beta_I", "beta_e", "beta_t", "beta_m", "beta_co",
    "beta_cc", "beta_minus3", "williams", "beta_rlb", "beta_sim", "beta_gl", "beta_z",
    "A_1", "A_2", "A_3", "A_4", "A_5", "A_6", "A_7", "A_8", "A_9", "A_10", "A_11", "A_12", "A_13", "A_14", "A_15",
    "A_16", "A_17", "A_18", "A_19", "A_20", "A_21", "A_22", "A_23", "A_24", "A_25", "A_26", "A_27", "A_28", "A_29",
    "A_30", "A_31", "A_32", "A_33", "C_7", "C_8", "A_36", "A_37", "A_38", "A_39", "A_40", "A_41", "A_42", "A_43",
    "S_8", "S_26",
    "Index_2", "Index_3", "Index_4", "Index_5", "Index_6", "Index_7", "Index_8", "Index_9", "Index_12",
    "Index_16", "Index_18", "Index_41", "Index_46", "Index_45",
    "BR", "NODF", "Sharedness", "Togetherness", "checker", "clumping", "combo", "cscore", "netCovariance", "sCoE", "vRatio"
  )

  val header = "mtxid\tstep\tmetric\tnm\tdist\tdir\tspecies\tplots\ttobsMetric\tlt\tgt\tmean\tsd\tses\tne_NaN_count\ttimestamp\n"

  if (!silent) print(header)
  commons.writeToFile(bw, header)
  var counter = 0

  for (loop <- 1 to n) {
    val stopWatch = new StopWatch()
    stopWatch.start()
    val mtx_UUID = java.util.UUID.randomUUID.toString
    val species = Random.nextInt(66) + 10
    val plots = Random.nextInt(66) + 10
//    val orgMtx = maximizePattern(mFactory.buildMatrix(species, plots, speciesConstraint = distribution), direction = direction)
    val orgMtx = commons.invertMatrix(maximizePattern(mFactory.buildMatrix(species, plots, speciesConstraint = distribution), direction = direction))

    var counter = 0

    val maxCount: Double = species * (plots / 2d)

    while (counter < maxCount) {
      counter += 1
      val nullDist = commons.runNullModels(orgMtx, sim, sim9Iter, metricsList)
      for (metric <- metricsList) {

        val obsMetricValue = commons.getMetricValue(orgMtx, metric)
        val expMean = commons.mean(nullDist(metric).filter(p => !p.isNaN))
        val expStDev = commons.stdDev(nullDist(metric).filter(p => !p.isNaN))
        val ses = (obsMetricValue.get - expMean) / expStDev
        val count = nullDist(metric).count(p => !p.isNaN).toDouble
        val lt = nullDist(metric).filter(p => !p.isNaN).count(v => v < obsMetricValue.get).toDouble / count
        val gt = nullDist(metric).filter(p => !p.isNaN).count(v => v > obsMetricValue.get).toDouble / count

        stopWatch.split()

        val outString = mtx_UUID + "\t" + counter + "\t" + metric + "\t" + sim + "\t" + distribution + "\t" + direction + "\t" + species + "\t" + plots + "\t" +
          formatter.format(obsMetricValue.getOrElse(0)) + "\t" +
          formatter.format(lt) + "\t" +
          formatter.format(gt) + "\t" +
          formatter.format(expMean) + "\t" +
          formatter.format(expStDev) + "\t" +
          formatter.format(ses) + "\t" +
          count.toInt + "\t" +
          stopWatch.toSplitString + "\n"

        if (!silent) print(outString)
        commons.writeToFile(bw, outString)
      }
      val randRow = Random.nextInt(orgMtx.length)
      orgMtx(randRow) = shuffle(orgMtx(randRow))
    }
  }

  private def maximizePattern(mtx: Array[Array[Int]], direction: String = "positive"): Array[Array[Int]] = {
    val workingMtx = mtx.map(_.clone)
    val maximizer = new MaximizeMetric

    direction match {
      case dir if dir.startsWith("pos") => maximizer.sMax(workingMtx, shuffled = false, fixEmpty = true)
      case dir if dir.startsWith("neg") => maximizer.maxEvenness(workingMtx)
      case dir if dir.startsWith("c") => maximizer.cMax(workingMtx, workingMtx.transpose.map(_.sum))
    }
  }

  /**
   * This can't be the FisherYates shuffle as it starts at one side by default. Since maximizing
   * the signal moves most of the incidences to one side of the matrix using the shuffle results in
   * rapid decrease in signal with the first shuffle.
   */
  private def shuffle(array: Array[Int]): Array[Int] = {
    val buf = new ArrayBuffer[Int] ++= array
    val size = array.length
    val zeros = array.zipWithIndex.filter(_._1 == 0).map(_._2)
    val ones = array.zipWithIndex.filter(_._1 == 1).map(_._2)

    // this is just in case we have a full row of occurrences (species at every site)
    val k = if (!zeros.isEmpty) {
      zeros(Random.nextInt(zeros.length))
    } else {
      Random.nextInt(ones.length)
    }
    val l = ones(Random.nextInt(ones.length))

    def swap(i1: Int, i2: Int) {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    swap(l, k)
    buf.toArray
  }

}
