#' @title Generate classifier calibration data.
#'
#' @description
#' A calibrated classifier is one where the predicted probability of a class closely matches the
#' rate at which that class occurs, e.g. for data points which are assigned a predicted probability
#' of class A of .8, approximately 80 percent of such points should belong to class A if the classifier
#' is well calibrated. This is estimated empirically by grouping data points with similar predicted
#' probabilities for each class, and plotting the rate of each class within each bin against the
#' predicted probability bins.
#'
#' @family generate_plot_data
#' @family calibration
#' @aliases CalibrationData
#'
#' @template arg_plotroc_obj
#' @param breaks [\code{character(1)} | \code{numeric}]\cr
#'   If \code{character(1)}, the algorithm to use in generating probability bins.
#'   See \code{\link{hist}} for details.
#'   If \code{numeric}, the cut points for the bins.
#'   Default is \dQuote{Sturges}.
#' @param groups [\code{integer(1)}]\cr
#'   The number of bins to construct.
#'   If specified, \code{breaks} is ignored.
#'   Default is \code{NULL}.
#' @param task.id [\code{character(1)}]\cr
#'   Selected task in \code{\link{BenchmarkResult}} to do plots for, ignored otherwise.
#'   Default is first task.
#'
#' @return [CalibrationData]. A \code{list} containing:
#'   \item{proportion}{[\code{data.frame}] with columns:
#'     \itemize{
#'       \item \code{Learner} Name of learner.
#'       \item \code{bin} Bins calculated according to the \code{breaks} or \code{groups} argument.
#'       \item \code{Class} Class labels (for binary classification only the positive class).
#'       \item \code{Proportion} Proportion of observations from class \code{Class} among all
#'         observations with posterior probabilities of class \code{Class} within the
#'         interval given in \code{bin}.
#'     }}
#'   \item{data}{[\code{data.frame}] with columns:
#'     \itemize{
#'       \item \code{Learner} Name of learner.
#'       \item \code{truth} True class label.
#'       \item \code{Class} Class labels (for binary classification only the positive class).
#'       \item \code{Probability} Predicted posterior probability of \code{Class}.
#'       \item \code{bin} Bin corresponding to \code{Probability}.
#'     }}
#'   \item{task}{[\code{\link{TaskDesc}}]\cr
#'     Task description.}
#'
#' @references Vuk, Miha, and Curk, Tomaz. \dQuote{ROC Curve, Lift Chart, and Calibration Plot.} Metodoloski zvezki. Vol. 3. No. 1 (2006): 89-108.
#' @export
generateCalibrationData = function(obj, breaks = "Sturges", groups = NULL, task.id = NULL)
  UseMethod("generateCalibrationData")
#' @export
generateCalibrationData.Prediction = function(obj, breaks = "Sturges", groups = NULL, task.id = NULL) {
  checkPrediction(obj, task.type = "classif", predict.type = "prob")
  generateCalibrationData.list(namedList("prediction", obj), breaks, groups, task.id)
}
#' @export
generateCalibrationData.ResampleResult = function(obj, breaks = "Sturges", groups = NULL, task.id = NULL) {
  obj = getRRPredictions(obj)
  checkPrediction(obj, task.type = "classif", predict.type = "prob")
  generateCalibrationData.Prediction(obj, breaks, groups, task.id)
}
#' @export
generateCalibrationData.BenchmarkResult = function(obj, breaks = "Sturges", groups = NULL, task.id = NULL) {
  tids = getBMRTaskIds(obj)
  if (is.null(task.id))
    task.id = tids[1L]
  else
    assertChoice(task.id, tids)
  obj = getBMRPredictions(obj, task.ids = task.id, as.df = FALSE)[[1L]]

  for (x in obj)
    checkPrediction(x, task.type = "classif", predict.type = "prob")
  generateCalibrationData.list(obj, breaks, groups, task.id)
}
#' @export
generateCalibrationData.list = function(obj, breaks = "Sturges", groups = NULL, task.id = NULL) {
  assertList(obj, c("Prediction", "ResampleResult"), min.len = 1L)
  ## unwrap ResampleResult to Prediction and set default names
  if (inherits(obj[[1L]], "ResampleResult")) {
    if (is.null(names(obj)))
      names(obj) = extractSubList(obj, "learner.id")
    obj = extractSubList(obj, "pred", simplify = FALSE)
  }
  assertList(obj, names = "unique")
  td = obj[[1L]]$task.desc

  out = lapply(obj, function(pred) {
    df = data.frame("truth" = getPredictionTruth(pred),
                    getPredictionProbabilities(pred, cl = getTaskClassLevels(td)))
    df = reshape2::melt(df, id.vars = "truth", value.name = "Probability", variable.name = "Class")

    if (is.null(groups)) {
      break.points = hist(df$Probability, breaks = breaks, plot = FALSE)$breaks
      df$bin = cut(df$Probability, break.points, include.lowest = TRUE, ordered_results = TRUE)
    } else {
      requirePackages("Hmisc", default.method = "load", why = "Equal width binning of probabilities.")
      assertInt(groups, lower = 2, upper = length(unique(df$Probability)))
      df$bin = Hmisc::cut2(df$Probability, g = groups, digits = 3)
    }
    proportion = plyr::ddply(df, "bin", function(x) {
      tab = table(x$Class, x$truth)
      s = rowSums(tab)
      ifelse(s == 0, 0, diag(tab) / s)
    })
    list(data = df, proportion = proportion)
  })
  data = plyr::ldply(lapply(out, function(x) x$data), .id = "Learner")
  proportion = plyr::ldply(lapply(out, function(x) x$proportion), .id = "Learner")
  if (length(td$class.levels) == 2L) {
    proportion = proportion[, -which(td$negative == colnames(proportion))]
    data = data[!data$Class == td$negative, ]
  }
  max_bin = sapply(strsplit(levels(proportion$bin), ",|]|)"), function(x) as.numeric(x[length(x)]))
  proportion$bin = ordered(proportion$bin, levels = levels(proportion$bin)[order(max_bin)])
  proportion = reshape2::melt(proportion, id.vars = c("Learner", "bin"),
                              value.name = "Proportion", variable.name = "Class")
  data$bin = ordered(data$bin, levels = levels(data$bin)[order(max_bin)])

  makeS3Obj("CalibrationData",
            proportion = proportion,
            data = data,
            task = td)
}
#' @title Plot calibration data using ggplot2.
#'
#' @description
#' Plots calibration data from \code{\link{generateCalibrationData}}.
#'
#' @family plot
#' @family calibration
#'
#' @param obj [\code{CalibrationData}]\cr
#'   Result of \code{\link{generateCalibrationData}}.
#' @param smooth [\code{logical(1)}]\cr
#'   Whether to use a loess smoother.
#'   Default is \code{FALSE}.
#' @param reference [\code{logical(1)}]\cr
#'   Whether to plot a reference line showing perfect calibration.
#'   Default is \code{TRUE}.
#' @param rag [\code{logical(1)}]\cr
#'   Whether to include a rag plot which shows a rug plot on the top which pertains to
#'   positive cases and on the bottom which pertains to negative cases.
#'   Default is \code{TRUE}.
#' @template ret_gg2
#' @export
#' @examples
#' \dontrun{
#' lrns = list(makeLearner("classif.rpart", predict.type = "prob"),
#'             makeLearner("classif.nnet", predict.type = "prob"))
#' fit = lapply(lrns, train, task = iris.task)
#' pred = lapply(fit, predict, task = iris.task)
#' names(pred) = c("rpart", "nnet")
#' out = generateCalibrationData(pred, groups = 3)
#' plotCalibration(out)
#'
#' fit = lapply(lrns, train, task = sonar.task)
#' pred = lapply(fit, predict, task = sonar.task)
#' names(pred) = c("rpart", "lda")
#' out = generateCalibrationData(pred)
#' plotCalibration(out)
#' }
plotCalibration = function(obj, smooth = FALSE, reference = TRUE, rag = TRUE) {
  assertClass(obj, "CalibrationData")
  assertFlag(smooth)
  assertFlag(reference)
  assertFlag(rag)

  obj$proportion$xend = length(levels(obj$proportion$bin))

  p = ggplot(obj$proportion, aes_string("bin", "Proportion", color = "Class", group = "Class"))
  p = p + scale_x_discrete(drop = FALSE)

  if (smooth)
    p = p + stat_smooth(se = FALSE, span = 2, method = "loess")
  else
    p = p + geom_point() + geom_line()

  if (length(unique(obj$proportion$Learner)) > 1L)
    p = p + facet_wrap(~ Learner)

  if (reference)
    p = p + geom_segment(aes_string(1, 0, xend = "xend", yend = 1), colour = "black", linetype = "dashed")

  if (rag) {
    top_data = obj$data[obj$data$truth == obj$data$Class, ]
    top_data$x = jitter(as.numeric(top_data$bin))
    p = p + geom_rug(data = top_data, aes_string("x", y = 1), sides = "t", alpha = .25)
    bottom_data = obj$data[obj$data$truth != obj$data$Class, ]
    bottom_data$x = jitter(as.numeric(bottom_data$bin))
    p = p + geom_rug(data = bottom_data, aes_string("x", y = 1), sides = "b", alpha = .25)
  }
  p = p + labs(x = "Probability Bin", y = "Class Proportion")
  p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
