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
#' plotROCCoords(coords)
#'
#' lrn1 = makeLearner("classif.lda", predict.type = "prob")
#' lrn2 = makeLearner("classif.ksvm", predict.type = "prob")
#' rdesc = makeResampleDesc("CV", iters = 10, predict = "both")
#' res = benchmark(list(lrn1, lrn2), list(pid.task, sonar.task), rdesc)
#' coords = getROCCoords(res)
#' plotROCCoords(coords)
#' }
#' @export
getROCCoords = function(obj, thresholds = 50L) {
  UseMethod("getROCCoords")
}

#' @export
getROCCoords.Prediction = function(obj, thresholds = 50L) {
  checkPrediction(obj, task.type = "classif", predict.type = "prob", binary = TRUE)
  pos = obj$task.desc$positive
  data = data.frame(truth = obj$data$truth, prob = getPredictionProbabilities(obj))
  data = sortByCol(data, "prob", asc = FALSE)
  n.pos = sum(data$truth == pos)
  n.neg = nrow(data) - n.pos
  data = data.frame(threshold = data$prob, tpr = cumsum(data$truth == pos)/n.pos, fpr = cumsum(data$truth != pos)/n.neg)
  # forcefully add th = 0 and th = 1
  data = rbind(data.frame(threshold = 1, tpr = 0, fpr = 0), data, data.frame(threshold = 0, tpr = 1, fpr = 1))
  data = data[!duplicated(data$thres, fromLast = TRUE),]
  rownames(data) = NULL
  makeS3Obj("ROCCoords", data = data, averaged = FALSE)
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
    tpr = vnapply(thres, function(i) sum(prob >= i & is.pos)/n.pos)
    fpr = vnapply(thres, function(i) sum(prob >= i & is.neg)/n.neg)
    cbind(thres, tpr, fpr)
  })
  data = ddply(coords, ~ set + thres, summarize, tpr = mean(tpr), fpr = mean(fpr))
  makeS3Obj("ROCCoords", data = data, averaged = TRUE)
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
      res[[k]] = data.frame(task = names(obj)[i], learner = names(x)[j], getROCCoords(x[[j]]$pred, thresholds)$data)
      k = k + 1L
    }
  }
  data = do.call(rbind, res)
  makeS3Obj("ROCCoords", data = data, averaged = TRUE)
}

#' @title Plot ROC coords with ggplot2.
#'
#' @description
#' This function plots the coordinates of ROC curves with ggplot2.
#'
#' @family roc
#' @export
plotROCCoords = function(coords, line.size = 0.5, coord.fixed = TRUE, xlab = NULL, ylab = NULL) {
  dat = coords$data
  if (is.null (xlab))
    xlab = paste0(ifelse(coords$averaged, "Average f", "F"), "alse positive rate")
  if (is.null (ylab))
    ylab = paste0(ifelse(coords$averaged, "Average t", "T"), "rue positive rate")
  n = c(learner = nlevels(dat$learner), task = nlevels(dat$task), set = nlevels(dat$set)) 
  vars = names(sort(n[n > 1L], decreasing = TRUE))
  color.var = if(!length(vars)) NULL else vars[1]
  pl = ggplot(dat, aes(x = fpr, y = tpr)) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey", size = line.size) +
    labs(y = ylab, x = xlab) +
    geom_path(size = line.size, aes_string(color = color.var, group = color.var))
  if (length(vars) == 2)
    pl = pl + facet_wrap(reformulate(vars[2L]))
  if (length(vars) == 3)
    pl = pl + facet_grid(reformulate(vars[2L], vars[3L]))
  if (coord.fixed)
    pl = pl + coord_fixed(ratio = 1)
  return(pl)
}
