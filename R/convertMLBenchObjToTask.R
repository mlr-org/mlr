#' @title Convert a machine learning benchmark / demo object from package mlbench to a task.
#'
#' @description
#' We auto-set the target column, drop any column which is called \dQuote{Id} and
#' convert logicals to factors.
#'
#' @param x (`character(1)`)\cr
#'   Name of an mlbench function or dataset.
#' @param n (`integer(1)`)\cr
#'   Number of observations for data simul functions.
#'   Note that for a few mlbench function this setting is not exactly respected by mlbench.
#'   Default is 100.
#' @param ... (any)\cr
#'   Passed on to data simul functions.
#' @export
#' @examples
#' print(convertMLBenchObjToTask("Ionosphere"))
#' print(convertMLBenchObjToTask("mlbench.spirals", n = 100, sd = 0.1))
convertMLBenchObjToTask = function(x, n = 100L, ...) {

  assertString(x)
  requirePackages("mlbench")
  id = x

  datasets = data(package = "mlbench")
  datasets = datasets$results[, "Item"]

  targets = c(
    Soybean = "Class",
    BostonHousing = "medv",
    BostonHousing2 = "medv",
    BreastCancer = "Class",
    DNA = "Class",
    Glass = "Type",
    HouseVotes84 = "Class",
    Ionosphere = "Class",
    LetterRecognition = "lettr",
    Ozone = "V4",
    PimaIndiansDiabetes = "diabetes",
    PimaIndiansDiabetes2 = "diabetes",
    Satellite = "classes",
    Servo = "Class",
    Shuttle = "Class",
    Sonar = "Class",
    Soybean = "Class",
    Vehicle = "Class",
    Vowel = "Class",
    Zoo = "type"
  )

  if (x %in% datasets) {
    # we load a data set
    ee = new.env()
    data(list = x, envir = ee)
    d = ee[[x]]
    d$Id = NULL
    target = targets[[x]]
    d = convertDfCols(d, logicals.as.factor = TRUE)
  } else {
    x = getFromNamespace(x, "mlbench")
    n = asCount(n)
    z = x(n = n, ...)
    d = as.data.frame(z)
    target = if (!is.null(z$classes)) "classes" else "y"
  }
  task = if (is.factor(d[, target])) {
    makeClassifTask(id = id, data = d, target = target)
  } else {
    makeRegrTask(id = id, data = d, target = target)
  }
  return(task)
}
