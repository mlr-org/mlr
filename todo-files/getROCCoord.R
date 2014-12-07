#' @title Extract coordinates of ROC curves.
#'
#' @description
#' This function extracts the coordinates of ROC curves. For resampled predictions, ROC curves are
#' averaged by threshold averaging, which is also the default method in package \code{ROCR}.
#'
#' @param thresholds [\code{integer}]\cr
#'   Number of thresholds at which ROC coordinates shall be averaged. This argument is only considered
#'   if the function is applied to objects of class \code{\link{ResamplePrediction}}, 
#'   \code{\link{ResampleResult}} or \code{\link{BenchmarkResult}}. Default is to use 50.
#' @family roc
#' @export
#' @examples
#' \dontrun{
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' mod = train(lrn, sonar.task)
#' ps = predict(mod, sonar.task)
#' coords = getROCCoord(ps)
#' library(ggplot2)
#' ggplot(coords, aes(x = FPR, y = TPR)) +
#' geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey", size = 0.5) +
#' geom_path() +
#' theme_bw() +
#' coord_fixed(ratio = 1)
#' 
#' lrn1 = makeLearner("classif.lda", predict.type = "prob")
#' lrn2 = makeLearner("classif.ksvm", predict.type = "prob")
#' rdesc = makeResampleDesc("CV", iters = 10, predict = "both")
#' res = benchmark(list(lrn1, lrn2), list(pid.task, sonar.task), rdesc)
#' coords = getROCCoord(res)
#' ggplot(coords, aes(x = FPR, y = TPR, group = learner, color = learner)) +
#' geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey", size = 0.5) +
#' geom_path() +
#' theme_bw() +
#' coord_fixed(ratio = 1) +
#' facet_grid(task ~ set)
#' }
#' @export
getROCCoord = function(obj, thresholds = 50L) {
  UseMethod("getROCCoord")
}

#' @export
getROCCoord.Prediction = function(obj){
  if(getTaskType(obj) != "classif") {
    stop("Task must be of type classif")
  }
  if(obj$predict.type != "prob"){
    stop("Predict.type must be prob")
  }
  pos = obj$task.desc$positive
  dat = obj$data[, c("truth", paste0("prob.", pos))]
  dat = dat[order(dat[,2L], decreasing = TRUE),]
  truth = dat[,1L]
  prob = dat[,2L]
  n.pos = sum(truth == pos)
  n.neg = sum(truth != pos)
  dat = data.frame(thres = prob, TPR = cumsum(truth == pos)/n.pos, FPR = cumsum(truth != pos)/n.neg)
  dat = rbind(data.frame(thres = 1, TPR = 0, FPR = 0), dat)
  dat = dat[!duplicated(dat$thres),]
  rownames(dat) = NULL
  dat
}

#' @export
getROCCoord.ResamplePrediction = function(obj, thresholds = 50L){
  if(getTaskType(obj) != "classif") {
    stop("Task must be of type classif")
  }
  if(obj$predict.type != "prob"){
    stop("Predict.type must be prob")
  }
  assertInt(thresholds)
  thres = seq(0, 1, len = thresholds)
  task = getTaskId(obj)
  pos = obj$task.desc$positive
  dat = obj$data[, c("truth", paste0("prob.", pos), "iter", "set")]
  names(dat)[2L] = "prob"
  getCoord = function(dat) {
    dat = lapply(split(dat, dat$iter), function(dat) {
      data.frame(thres,
      TPR = sapply(thres, function(i) with(dat, sum(prob >= i & truth == pos)/sum(truth == pos))),
      FPR = sapply(thres, function(i) with(dat, sum(prob >= i & truth != pos)/sum(truth != pos))))
    })
    dat = do.call(rbind, dat)
    aggregate(cbind(TPR, FPR) ~ thres, dat, mean)
  }
  coord = lapply(split(dat, dat$set), getCoord)
  coord = do.call(rbind, coord)
  rownames(coord) = NULL
  data.frame(task, set = rep(levels(dat$set), each = thresholds), coord, stringsAsFactors = FALSE)
}

#' @export
getROCCoord.ResampleResult = function(obj, thresholds = 50L){
  getROCCoord(obj$pred, thresholds)
}

#' @export
getROCCoord.BenchmarkResult = function(obj, thresholds = 50L){
  task.names = names(obj)
  learner.names = unname(lapply(obj, names))
  df = data.frame(
    task = rep.int(task.names, viapply(learner.names, length)),
    learner = unlist(learner.names),
    stringsAsFactors = FALSE
  )
  coord = rowLapply(df, function(x) {
    data.frame(learner = x$learner, getROCCoord(obj[[x$task]][[x$learner]]$pred, thresholds),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, coord)
}
