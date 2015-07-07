#FIXME: where does time exactly come from? only test preds?

#' Prediction from resampling.
#'
#' Contains predictions from resampling, returned (among other stuff) by function \code{\link{resample}}.
#' Can basically be used in the same way as \code{\link{Prediction}}, its super class.
#' The main differences are:
#' (a) The internal data.frame (member \code{data}) contains an additional column \code{iter}, specifying the iteration
#' of the resampling strategy, and and additional columns \code{set}, specifying whether the prediction
#' was from an observation in the \dQuote{train} or \dQuote{test} set. (b) The prediction \code{time} is
#' a numeric vector, its length equals the number of iterations.
#' @name ResamplePrediction
#' @rdname ResamplePrediction
#' @family resample
NULL


makeResamplePrediction = function(instance, preds.test, preds.train) {
  tenull = sapply(preds.test, is.null)
  trnull = sapply(preds.train, is.null)
  if(any(tenull)) pr.te = preds.test[!tenull] else pr.te = preds.test
  if(any(trnull)) pr.tr = preds.train[!trnull] else pr.tr = preds.train

  #   dtest = do.call("rbind", lapply(seq_along(pr.te), function(X)
  #     cbind(pr.te[[X]]$data, iter = X, set = "test") ))
  #   dtrain = do.call("rbind", lapply(seq_along(pr.tr), function(X)
  #     cbind(pr.tr[[X]]$data, iter = X, set = "train") ))

  dtest = plyr::rbind.fill(lapply(seq_along(pr.te), function(X)
    cbind(pr.te[[X]]$data, iter = X, set = "test") ))
  dtrain = plyr::rbind.fill(lapply(seq_along(pr.tr), function(X)
    cbind(pr.tr[[X]]$data, iter = X, set = "train") ))

  data = rbind(dtest, dtrain)

  p1 = preds.test[[1L]]
  makeS3Obj(c("ResamplePrediction", class(p1)),
    instance = instance,
    predict.type = p1$predict.type,
    data = data,
    threshold = p1$threshold,
    task.desc = p1$task.desc,
    time = extractSubList(preds.test, "time")
  )
}

#' @export
print.ResamplePrediction = function(x, ...) {
  cat("Resampled Prediction for:\n")
  print(x$instance$desc)
  catf("predict.type: %s", x$predict.type)
  catf("threshold: %s", collapse(sprintf("%s=%.2f", names(x$threshold), x$threshold)))
  catf("time (mean): %.2f", mean(x$time))
  print(head(as.data.frame(x)))
}
