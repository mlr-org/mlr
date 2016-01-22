#' regression using randomForest.
#'
#' a mlr learner for regrssion tasks using \code{\link[randomForest]{randomForest}}.
#'
#' if \code{predict.type = "se"} the \code{se.method} (by default \dQuote{jackknife})
#' is estimated, using the methods described in Sexton and Laake (2009).
#'
#' If \code{se.method = "bootstrap"} the standard error of a prediction is estimated by bootstrapping the random forest, where the number of bootstrap replicates and the number of trees in the ensemble are controlled by \code{se.boot} and \code{ntree.for.se} respectively, and then taking the standard deviation of the predictions.
#'
#' If \code{se.method = "jackknife"}, the default, the standard error of a prediction is by computing the jackknife-after-bootstrap, the mean-squared difference between the prediction made by only using trees which did not contain said observation and the ensemble prediction.
#'
#' For both \dQuote{jackknife} and \dQuote{bootstrap} a Monte-Carlo bias correction is applied and, in the case that this results in a negative variance estimate, the values are truncated at 0.
#'
#' @references [Joseph Sexton] and [Petter Laake],; [Standard errors for bagged and random forest estimators], Computational Statistics and Data Analysis Volume 53, 2009, [801-811].
#'
#' @name regr.randomForest
NULL

#' @export
makeRLearner.regr.randomForest = function() {
  makeRLearnerRegr(
    cl = "regr.randomForest",
    package = "randomForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "ntree.for.se", default = 100L, lower = 1L),
      makeDiscreteLearnerParam(id = "se.method", default = "jackknife",
                               values = c("bootstrap", "jackknife",  "sd"),
                               requires = quote(se.method %in% c("jackknife") && keep.inbag == TRUE)),
      makeIntegerLearnerParam(id = "se.boot", default = 50L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeIntegerLearnerParam(id = "sampsize", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 5L, lower = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeLogicalLearnerParam(id = "importance", default = FALSE),
      makeLogicalLearnerParam(id = "localImp", default = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(
      se.method = "bootstrap",
      se.boot = 50L,
      ntree.for.se = 100L
    ),
    properties = c("numerics", "factors", "ordered", "se"),
    name = "Random Forest",
    short.name = "rf",
    note = "See `?regr.randomForest` for information about se estimation."

  )
}

#' @export
trainLearner.regr.randomForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  if (.learner$predict.type == "se" &
        .learner$par.vals$se.method == "bootstrap") {
    base.lrn = setPredictType(.learner, "response")
    base.lrn = setHyperPars(base.lrn, ntree = .learner$par.vals$ntree.for.se)
    bag.rf = makeBaggingWrapper(base.lrn, .learner$par.vals$se.boot, bw.replace = TRUE)
    m = train(bag.rf, .task, .subset, .weights)
  } else {
    data = getTaskData(.task, .subset, target.extra = TRUE)
    m = randomForest::randomForest(x = data[["data"]], y = data[["target"]], ...)
  }
  return(m)
}


#' @export
predictLearner.regr.randomForest = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "se") {
    se.fun = switch(.learner$par.vals$se.method,
      bootstrap = bootstrapStandardError,
      jackknife = jackknifeStandardError,
      sd = sdStandardError
    )
    se.fun(.learner, .model, .newdata, ...)
  } else {
    predict(.model$learner.model, newdata = .newdata, ...)
  }
}

# Computes brute force or noisy bootstrap
# Set ntree = ntree.for.se for the brute force bootstrap
# Set ntree.for.se << ntree for the noisy bootstrap (mc bias corrected)
bootstrapStandardError = function(.learner, .model, .newdata, ...) {
  pred.all.boot = lapply(getLearnerModel(.model$learner.model), function(x)
    predict(x$learner.model, newdata = .newdata, predict.all = TRUE)$individual)
  B = .learner$par.vals$se.boot
  R = .learner$par.vals$ntree.for.se
  M = .learner$par.vals$ntree
  bias = ((1 / R) - (1 / M)) / (B * R * (R - 1)) *
    rowSums(matrix(sapply(pred.all.boot, function(p) rowSums((p - mean(p))^2)),
                   nrow = nrow(.newdata), ncol = R, byrow = TRUE))
  pred = getPredictionResponse(predict(.model$learner.model, newdata = .newdata))
  pred.boot = lapply(getLearnerModel(.model$learner.model), predict, newdata = .newdata, ...)
  pred.boot = extractSubList(pred.boot, c("data", "response"))
  if (is.vector(pred.boot)) {
    pred.boot = matrix(pred.boot, nrow = nrow(.newdata), ncol = R, byrow = TRUE)
  }
  var.boot = apply(pred.boot, 1, var) - bias
  var.boot[var.boot <= 0] = 0
  cbind(pred, sqrt(var.boot))
}

# Computes the mc bias-corrected jackknife after bootstrap
jackknifeStandardError = function(.learner, .model, .newdata, ...) {
  model = .model$learner.model
  n = nrow(model$inbag)
  ntree = model$ntree
  pred = predict(model, newdata = .newdata, predict.all = TRUE)
  oob = t(sapply(seq_len(n), function(i) model$inbag[i, ] == 0))
  jack_n = apply(oob, 1, function(x) rowMeans(pred$individual[, x, drop = FALSE]))
  if (is.vector(jack_n)) {
    jack_n = t(as.matrix(jack_n))
  }
  jack = (n - 1) / n * rowSums((jack_n - pred$aggregate)^2)
  bias = (exp(1) - 1) * n / ntree^2 * rowSums((pred$individual - pred$aggregate)^2)
  jab = jack - bias
  jab[jab < 0] = 0
  return(cbind(pred$aggregate, sqrt(jab)))
}

# computes the standard deviation across trees
sdStandardError = function(.learner, .model, .newdata, ...) {
  pred = predict(.model$learner.model, newdata = .newdata, predict.all = TRUE, ...)
  se = apply(pred$individual, 1, sd)
  return(cbind(pred$aggregate, se))
}
