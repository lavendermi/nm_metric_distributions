package ca.mikelavender.nm_metric_evaluations

import ca.mikelavender.associations.utilities.MatrixFactory
import org.apache.commons.lang3.time.StopWatch

import java.io.{BufferedWriter, File, FileWriter}
import java.text.DecimalFormat
import scala.util.Random

class typeI(sim: String,
            sim9Iter: Int,
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
  val commons = new CommonMethods


  //  val file = new File(pathVar + "raw_data/typeI/inverted_typeI_" +
  val file = new File(pathVar + "raw_data/typeI/typeI_" +
    distribution + "_" + sim +  "_" + java.util.UUID.randomUUID.toString + ".tsv")

  val bw = new BufferedWriter(new FileWriter(file))

  val metricsList = List("b", "c", "d",
    "beta_w", "beta_c", "beta_wb", "beta_r", "beta_I", "beta_e", "beta_t", "beta_m", "beta_co",
    "beta_cc", "beta_minus3", "williams", "beta_rlb", "beta_sim", "beta_gl", "beta_z",
    "A_1", "A_2", "A_3", "A_4", "A_5", "A_6", "A_7", "A_8", "A_9", "A_10", "A_11", "A_12", "A_13", "A_14", "A_15",
    "A_16", "A_17", "A_18", "A_19", "A_20", "A_21", "A_22", "A_23", "A_24", "A_25", "A_26", "A_27", "A_28", "A_29",
    "A_30", "A_31", "A_32", "A_33", "C_7", "C_8", "A_36", "A_37", "A_38", "A_39", "A_40", "A_41", "A_42", "A_43",
    "S_8", "S_26",
    "Index_2", "Index_3", "Index_4", "Index_5", "Index_6", "Index_7", "Index_8", "Index_9", "Index_12", "Index_12_b",
    "Index_16", "Index_18", "Index_41", "Index_46", "Index_45",
    "BR", "NODF", "Sharedness", "Togetherness", "checker", "clumping", "combo", "cscore", "netCovariance", "sCoE", "vRatio"
  )

  val header = "id\tmetric\tnm\tdist\tspecies\tplots\tobsMetric\tlt\tgt\tmean\tsd\tses\tne_NaN_count\ttimestamp\n"

  if (!silent) print(header)
  commons.writeToFile(bw, header)
  var counter = 0

  val stopWatch = new StopWatch()
  stopWatch.start()
  for (loop <- 1 to n) {
    val species = Random.nextInt(66) + 10
    val plots = Random.nextInt(66) + 10
    counter += 1
    val orgMtx = mFactory.buildMatrix(species, plots, speciesConstraint = distribution)
//    val orgMtx = commons.invertMatrix(mFactory.buildMatrix(species, plots, speciesConstraint = distribution))


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

      val outString = counter + "\t" + metric + "\t" + sim + "\t" + distribution + "\t" + species + "\t" + plots + "\t" +
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
  }
  stopWatch.stop()

}
