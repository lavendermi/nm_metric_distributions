package ca.mikelavender.nm_metric_evaluations

object Main extends App {
  // initialize everything needed
  val test = args(0)
  val sim = args(1)
  val species = args(2).toInt
  val plots = args(3).toInt
  val silent = args(4).toBoolean
  val n = args(5).toInt
  val distribution = args(6)
  val direction = args(7) match {
    case s if s.startsWith("p") => "positive"
    case s if s.startsWith("n") => "negative"
  }

  // This pathVar needs to be set to match the system it is running on! Unfortunately, the way I setup the project I
  // can't use relative paths. It should point to the folder that the "01-raw_data" folder is in.
//  val pathVar = "REPLACE_WITH_PATH_TO FOLDER THAT CONTAINS THE \"01-raw_data\" FOLDER"
  val pathVar = "out/"

  test match {
    case "typeI" => new typeI(sim, sim9Iter = 5 * List(species, plots).min, silent, n, distribution, pathVar)
    case "typeII" => new typeII(sim, sim9Iter = 5 * List(species, plots).min, silent, n, distribution, direction, pathVar)
    case "noise" => new noise(sim, sim9Iter = 5 * List(species, plots).min, silent, n, distribution, direction, pathVar)
    case _ => sys.error("No test specified")
  }
}