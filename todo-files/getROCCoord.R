#' @title Extract coordinates of ROC curves.
#'
#' @description
#' This function extracts the coordinates of ROC curves. For resampled predictions, ROC curves are
#' averaged by threshold averaging, which is also the default method in package \code{ROCR}.
#'
#' @param thresholds [\code{integer}]\cr
#'   Number of thresholds at which ROC coordinates shall be averaged. This argument is only considered
#'   if the function is applied to objects of class \code{\link{ResamplePrediction}},
#'   \code{\link{ResampleResult}} or \code{\link{BenchmarkResult}}.
#'   Default is to use 50.
#' @family roc
#' @export
#' @examples
#' \dontrun{
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' mod = train(lrn, sonar.task)
#' ps = predict(mod, sonar.task)
#' coords = getROCCoords(ps)
#' library(ggplot2)
#' ggplot(coords, aes(x = fpr, y = tpr)) +
#' geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey", size = 0.5) +
#' geom_path() +
#' theme_bw() +
#' coord_fixed(ratio = 1)
#'
#' lrn1 = makeLearner("classif.lda", predict.type = "prob")
#' lrn2 = makeLearner("classif.ksvm", predict.type = "prob")
#' rdesc = makeResampleDesc("CV", iters = 10, predict = "both")
#' res = benchmark(list(lrn1, lrn2), list(pid.task, sonar.task), rdesc)
#' coords = getROCCoords(res)
#' ggplot(coords, aes(x = fpr, y = tpr, group = learner, color = learner)) +
#' geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey", size = 0.5) +
#' geom_path() +
#' theme_bw() +
#' coord_fixed(ratio = 1) +
#' facet_grid(task ~ set)
#' }
#' @export
getROCCoords = function(obj, thresholds = 50L) {
  UseMethod("getROCCoords")
}

#' @export
getROCCoords.Prediction = function(obj, thresholds = 50L) {
  checkPrediction(obj, task.type = "classif", predict.type = "prob", binary = TRUE)
  pos = obj$task.desc$positive
  dat = data.frame(truth = obj$data$truth, prob = getProbabilities(obj))
  dat = sortByCol(dat, "prob", asc = FALSE)
  n.pos = sum(dat$truth == pos)
  n.neg = nrow(dat) - n.pos
  # FIXME: BB: is the calculation here really correct for ties? especially if
  # we later remove dupliates (later dupls in the order)? this MUST be commented.
  # actually: we rather need a test here
  dat = data.frame(threshold = dat$prob, tpr = cumsum(dat$truth == pos)/n.pos, fpr = cumsum(dat$truth != pos)/n.neg)
  # forcefully add th = 0 and th = 1
  dat = rbind(data.frame(threshold = 1, tpr = 0, fpr = 0), dat, data.frame(threshold = 0, tpr = 1, fpr = 1))
  dat = dat[!duplicated(dat$thres),]
  setRowNames(dat, NULL)
}

#' @export
getROCCoords.ResamplePrediction = function(obj, thresholds = 50L) {
  checkPrediction(obj, task.type = "classif", predict.type = "prob", binary = TRUE)
  pos = obj$task.desc$positive
  thresholds = asInt(thresholds)
  thres = seq(0, 1, len = thresholds)
  coords = ddply(obj$data, ~ set + iter, function(df) {
    is.pos = df$truth == pos
    is.neg = !is.pos
    n.pos = sum(is.pos)
    n.neg = sum(is.neg)
    prob = df[, paste0("prob.", pos)]
    cbind(thres, tpr = vnapply(thres, function(i) sum(prob >= i & is.pos)/n.pos),
      fpr = vnapply(thres, function(i) sum(prob >= i & is.neg)/n.neg))
  })
  coords = ddply(coords, ~ set + thres, summarize, tpr = mean(tpr), fpr = mean(fpr))
  data.frame(task = getTaskId(obj), coords, stringsAsFactors = FALSE)
}

#' @export
getROCCoords.ResampleResult = function(obj, thresholds = 50L) {
  getROCCoords(obj$pred, thresholds)
}

#' @export
getROCCoords.BenchmarkResult = function(obj, thresholds = 50L) {
  res = list()
  k = 1L
  # for each task / learner combo rbind the df to res
  for (i in seq_along(obj)) {
    x = obj[[i]]
    for (j in seq_along(x)) {
      res[[k]] = data.frame(learner = names(x)[i], getROCCoords(x[[j]]$pred, thresholds))
      k = k + 1L
    }
  }
  do.call(rbind, res)
}
